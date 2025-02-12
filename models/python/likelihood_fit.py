import pickle
import numpy as np
import pandas as pd
from scipy.optimize import differential_evolution
import statsmodels.api as sm
import pymc as pm

import logging

import jax.numpy as jnp
import numpyro
import numpyro.distributions as dist
from numpyro.infer import MCMC, NUTS, Predictive
import numpy as np
from jax.scipy.special import expit, logit
import matplotlib.pyplot as plt
import jax
from numpyro import sample, plate
import arviz as az
from tqdm import tqdm

custom_colors = ['purple', 'y', 'red', 'blue']

# Set the global color cycle
plt.rcParams['axes.prop_cycle'] = plt.cycler(color=custom_colors)

MODE = "individual"
FIT = True
CONDITION = "Without" # With feedback / without feedback
SLUG = 'rw_with_ic'

logging.basicConfig(level=logging.INFO)

def initialize_parameters():
    a = np.random.normal(0, 0.01)
    b = np.random.normal(0, 0.01)
    c = np.random.normal(0, 0.01)
    return a, b, c

# Define the Bayesian logistic regression with Beta likelihood
def bayesian_beta_regression(X, y=None):
    # Priors for intercept and coefficient
    intercept = numpyro.sample("intercept", dist.Cauchy(0, 2.5))  # Prior on intercept
    beta = numpyro.sample("beta", dist.Cauchy(0, 2.5))  # Prior on the coefficient
    
    # Linear combination (log-odds)
    logits = intercept + beta * X
    
    # Convert logits to probabilities using the logistic (sigmoid) function
    prob = expit(logits)
    
    # Likelihood function: Using Beta distribution for continuous probabilities in [0, 1]
    # Adding a small epsilon to avoid exactly 0 or 1
    epsilon = 1e-6
    y_clipped = jnp.clip(y, epsilon, 1 - epsilon)
    alpha = prob * 10  # Shape parameter for Beta distribution
    beta_param = (1 - prob) * 10  # Shape parameter for Beta distribution
    
    # Beta likelihood for continuous probabilities in [0, 1]
    numpyro.sample("obs", dist.Beta(alpha, beta_param), obs=y_clipped)

# Function to compute Q-value using the quadratic model
def compute_Q_value(x, a, b, c):
    return x @ np.array([a, b, c])  # Quadratic formula to predict Q-value

# Sigmoid-like function to map Q-values to confidence (0 to 100)
def map_Q_to_confidence(Q, kappa=1):
    return 1 / (1 + np.exp(-Q))

def nonlinear_transform(x, k=1):
    return x**k / (x**k + (1 - x)**k)

if __name__ == "__main__":

    rng_next = jax.random.PRNGKey(0)

    ### fitting RW model to participant responses
        
    glob_df = pd.read_csv('data/df_main_task_exclude.csv')
    # glob_df = pd.read_csv('data/df_model_predictions_without_ic.csv')

    # Collect predictions in the original DataFrame
    predictions_dict = {}

    learning_rates_a = {}
    learning_rates_b = {}
    learning_rates_c = {}

    def fit_single(fit_df, 
                   learning_rate_a, 
                   learning_rate_b, 
                   learning_rate_c, 
                   kappa, 
                   plot=False):
        # Initialize parameters for the quadratic model
        a, b, c = initialize_parameters()  # Quadratic coefficients: Q(x) = ax^2 + bx + c

        # Extract necessary columns from fit_df
        observations = fit_df['num_blue_news'].to_numpy()  # Number of blue endorsements
        # squared_observations = observations**2
        # std_squared_obs = (squared_observations - np.mean(squared_observations)) / np.std(squared_observations)
        # std_obs = (observations - np.mean(observations)) / np.std(observations)
        # std_obs = observations
        hidden_states = fit_df['correct_state'].map({'blue': 1, 'green': 0})  # True hidden states
        confidences = fit_df['response'].to_numpy() / 100  # Confidence ratings normalized to [0,1]
        def log_likelihood_single(a, b, c, true_state, observation):
            predicted_Q = compute_Q_value(observation, a, b, c)
            predicted_confidence = map_Q_to_confidence(predicted_Q, kappa)
            return np.log(predicted_confidence + 1e-12) if true_state == 1 else np.log(1 - predicted_confidence + 1e-12)

        likelihood = 0
        num_trials = len(fit_df)
        predicted_confidences = []
        prediction_errors = []
        accuracies = []
        history_of_a = [a]
        history_of_b = [b]
        history_of_c = [c]
        history_of_Q = []

        # Main training loop for Rescorla-Wagner model
        mean_obs = np.mean(observations)
        std_obs = np.std(observations)
        mean_squared_obs = np.mean(observations**2)
        std_squared_obs = np.std(observations**2)
        normalizer = np.array([mean_squared_obs, mean_obs, 0])
        scaler = np.array([std_squared_obs, std_obs, 1])
        obs_control = None
        for t in range(num_trials):
            x = observations[t]
            obs_vector = np.array([x**2, x, 1])
            obs_vector = (obs_vector - normalizer) / scaler
            hidden_state = hidden_states.iloc[t]

            if t == 0:
                obs_control = obs_vector

            # Predict Q-value and confidence
            predicted_Q = compute_Q_value(obs_vector, a, b, c)
            predicted_confidence = map_Q_to_confidence(predicted_Q)
            predicted_confidences += [predicted_confidence]

            # Calculate log-likelihood # TODO: use likelihood function to maximize likelihood under participant choice
            participant_choice = int(confidences[t] > 0.5)
            likelihood += log_likelihood_single(a, b, c, participant_choice, obs_vector)
            # breakpoint()

            # Compute prediction error and update parameters
            # prediction_error = confidences[t] - predicted_confidence # TODO: no non-linear transformation
            # prediction_error = int(confidences[t] > 0.5) - int(predicted_confidence > 0.5) # choice fit
            prediction_error_hid = hidden_state - predicted_confidence # TODO: Make choices binarized

            prediction_error = prediction_error_hid
            # breakpoint()
            a = a + learning_rate_a * prediction_error * obs_vector[0]
            history_of_Q += [learning_rate_a * prediction_error * obs_vector[0]]
            b = b + learning_rate_b * prediction_error * obs_vector[1]
            c = c + learning_rate_c * prediction_error

            history_of_a += [a]
            history_of_b += [b]
            history_of_c += [c]
            # history_of_Q += [compute_Q_value(obs_control, a, b, c)]
            prediction_errors += [prediction_error]
            accuracies += [hidden_state == int(predicted_confidence > 0.5)]
        
        if not plot:
            return likelihood, predicted_confidences
        else:
            # import matplotlib.pyplot as plt
            # plt.plot(np.arange(0, 6), [map_Q_to_confidence(compute_Q_value(x, a, b, c)) for x in range(6)], label='Q-value')
            # plt.show()
            # plt.close()
            # print(likelihood)
            import matplotlib.pyplot as plt
            fig, (ax1, ax2, ax3) = plt.subplots(3)
            ax1.plot(history_of_a, label='a')
            ax1.plot(history_of_b, label='b')
            ax1.plot(history_of_c, label='c')

            ax2.plot(history_of_Q, label='Q')
            # ax3.plot(prediction_errors, label='prediction_error')
            # ax3.plot(np.abs(prediction_errors), label='absolute_prediction_error')
            """plot rolling average of prediction errors"""
            ax3.plot(np.cumsum(accuracies), label='model accuracy')
            ax3.plot(((confidences > 0.5).astype(int) == hidden_states).cumsum().to_numpy(), label='data accuracy')
            # ax3.plot(np.cumsum())
            
            ax1.legend()
            ax3.legend()
            plt.show()
            breakpoint()
            return likelihood, predicted_confidences

    def wrap_fit_single(args, fit_df):
        return -fit_single(fit_df, *args)[0]

    def fit_all_conditions(args, fit_df_for_participant):
        """For one participant fit oer all conditions"""
        neg_likelihood = 0
        for source_type, g_df in fit_df_for_participant.groupby(by='source_type'):
            fit_df = g_df.sort_values(by='trial_number')
            neg_likelihood += wrap_fit_single(args, fit_df)
        return neg_likelihood
    
    if FIT:
        glob_df = glob_df[(glob_df['block_stage'] != 'station_only') & \
                          (glob_df['condition'] == CONDITION)]

        # Main loop for each source_type
        for completion_code, g_df in tqdm(glob_df.groupby(by='completion_code')):
        # for source_type in ['bluebias']:
            PLOT = False
            # this_df = g_df[
            #     (g_df['block_stage'] != 'station_only') & \
            #     (g_df['condition'] == CONDITION)
            # ]
            # breakpoint()
            # Optimize the parameters using differential evolution
            res = differential_evolution(
                lambda args: fit_all_conditions(args, g_df),
                bounds=[(1e-6, 10), (1e-6, 10), (1e-6, 10), (1e-2, 10)],
                maxiter=250
            )
            # breakpoint()
            
            if (res.x > 0.9).any() and PLOT:
                plot = True
            else:
                plot = False

            for source_type, fit_df in g_df.groupby(by='source_type'):

                try:
                    learning_rates_a[source_type].append(res.x[0])
                    learning_rates_b[source_type].append(res.x[1])
                    learning_rates_c[source_type].append(res.x[2])
                except KeyError:
                    learning_rates_a[source_type] = [res.x[0]]
                    learning_rates_b[source_type] = [res.x[1]]
                    learning_rates_c[source_type] = [res.x[2]]
                # breakpoint()
                # Fit the model with the optimized parameters

                __, predicted_confidences = fit_single(fit_df, *res.x, plot=plot)
                # __, predicted_confidences = fit_single(fit_df, 0.01, 1)
                # breakpoint()

                # Generate predictions based on the optimized parameters
                observations = fit_df['num_blue_news'].to_numpy()
                predictions = predicted_confidences

                # Store predictions for this completion_code in a dictionary
                predictions_dict[completion_code] = predictions
                # Add predictions to the original DataFrame
                glob_df.loc[(glob_df['source_type'] == source_type) & \
                (glob_df['block_stage'] != 'station_only') & \
                (glob_df['condition'] == CONDITION) & \
                (glob_df['completion_code'] == completion_code), 'posterior_blue_I'] = predictions

                glob_df.loc[(glob_df['source_type'] == source_type) & \
                (glob_df['block_stage'] != 'station_only') & \
                (glob_df['condition'] == CONDITION) &
                (glob_df['completion_code'] == completion_code), 'X_F'] = observations

                glob_df.loc[(glob_df['source_type'] == source_type) & \
                (glob_df['block_stage'] != 'station_only') & \
                (glob_df['condition'] == CONDITION) &
                (glob_df['completion_code'] == completion_code), 'X_I'] = observations

                glob_df.loc[(glob_df['source_type'] == source_type) & \
                (glob_df['block_stage'] != 'station_only') & \
                (glob_df['condition'] == CONDITION) & \
                (glob_df['completion_code'] == completion_code), 'ground_truth'] = fit_df['correct_state'].map({'blue': 1, 'green': -1})

        with open("params.pkl", 'wb') as f:
            pickle.dump(glob_df, f)
        
        with open(f"learning_rates_a_{CONDITION}.pkl", 'wb') as f:
            pickle.dump(learning_rates_a, f)
        
        with open(f"learning_rates_b_{CONDITION}.pkl", 'wb') as f:
            pickle.dump(learning_rates_b, f)
        
        with open(f"learning_rates_c_{CONDITION}.pkl", 'wb') as f:
            pickle.dump(learning_rates_c, f)

    else:
        # glob_df = pickle.load(open("params.pkl", 'rb'))
        # glob_df = pd.read_csv('data/df_main_task_wide_exclude.csv')
        # glob_df = pd.read_csv('data/df_model_predictions_without_ic.csv')
        glob_df = pd.read_csv('data/df_model_predictions_with_ic.csv')
        
    # breakpoint()
    if MODE == "pooled":
        df = glob_df
        # df = pd.read_csv('data/df_model_predictions_without_ic.csv')
        # this_df = df[ (df['completion_code'] == 'oovk5u7v') & (df['source_type']== 'bluebias') & (df['plugin_type'] == 'full_source')].sort_values(by='trial_number')

        import matplotlib.pyplot as plt
        fig, axs = plt.subplots(3, figsize=(10, 20))
        fig, axs_lrs = plt.subplots(3, figsize=(10, 20))
        
        glob_intercepts = []
        glob_slopes = []

        for source_type in ['helpful', 'random', 'opposite', 'bluebias']:
            
            glob_df = df
            
            # try: 
            logging.warning(f"Before removing NAN values: {glob_df.shape}")
            this_df = glob_df[ 
                (glob_df['source_type'] == source_type) & \
                    (glob_df['block_stage'] != 'station_only') & \
                        (glob_df['condition'] == CONDITION) & \
                            (glob_df['with_independent_council'] == False)] # .sort_values(by='trial_number_within_block')
            ## clean NAN values from the dataset
            # this_df = this_df.groupby(by='completion_code').filter(lambda x: ~ (x.isna().all().any()))
            # breakpoint()

            logging.warning(f"After removing NAN values: {this_df.shape}")

            intercepts = []
            slopes = []
            accuracy = []

            for group, g_df in this_df.groupby(by='block_quarter'):

                list_of_blue_endorsements = g_df['X_F'] - 2.5
                list_of_hidden_states = ((g_df['ground_truth'] + 1) / 2).to_numpy()
                list_of_confidences = g_df['posterior_blue_I']
                
                # Add a constant term for the intercept
                # features = np.column_stack((np.arange(len(list_of_confidences)), list_of_blue_endorsements))
                features = np.array(list_of_blue_endorsements)
                features_with_intercept = sm.add_constant(features)
                probabilities = list_of_confidences

                # # Fit a GLM with a binomial family (logistic regression)
                # glm_binom = sm.GLM(probabilities, features_with_intercept, family=sm.families.Binomial())
                # glm_results = glm_binom.fit()
                # # Print the summary of the GLM results
                # # print(glm_results.summary())

                # intercepts.append(glm_results.params[0])
                # slopes.append(glm_results.params[1])
                # continue 
                # Define the Bayesian logistic regression model for probabilities
                def logistic_regression_prob_model(X, y_log_prob=None):
                    # Priors for the coefficients (Cauchy priors)
                    intercept = sample('intercept', dist.Cauchy(0.,2.5))
                    beta = sample('beta', dist.Cauchy(0., 10))

                    # Linear model: logits = intercept + X @ beta
                    logits = intercept + jnp.dot(X, beta)
                    # prob = expit(logits)
                    
                    with plate('data', X.shape[0]):
                        sample('obs', dist.Normal(logits, 1.6*2.5 * 6.5), obs=y_log_prob)
                        # sample('obs', dist.Beta(5 * prob, 5 * (1 - prob)), obs=y_prob)
                    
                rng_next, rng_nuts, rng_predict = jax.random.split(rng_next, 3)
                # Set up the NUTS sampler
                nuts_kernel = NUTS(logistic_regression_prob_model)
                # nuts_kernel = NUTS(bayesian_beta_regression)

                # Run MCMC to sample from the posterior
                y_prob = jnp.array(probabilities)
                eps = 1e-2
                y_prob = jnp.clip(y_prob, eps, 1 - eps)
                y_log_prob = jnp.log(y_prob / (1 - y_prob))
                # breakpoint()
                X = jnp.array(features)
                
                mcmc = MCMC(nuts_kernel, num_warmup=100, num_samples=100)
                mcmc.run(rng_nuts, X=X, y_log_prob=y_log_prob)
                # mcmc.run(rng_nuts, X=X, y=y_prob)

                # Get the posterior samples
                posterior_samples = mcmc.get_samples()

                # Summary of the posterior
                mcmc.print_summary()

                # data = az.from_numpyro(mcmc)
                # az.plot_trace(data, compact=True, figsize=(15, 45));
                # plt.show()
                # exit()

                intercepts.append(posterior_samples['intercept'].mean())
                slopes.append(posterior_samples['beta'].mean())
                # Binarize predictions and compare with ground truth
                predictive = Predictive(logistic_regression_prob_model, posterior_samples, return_sites=['obs'])
                predictions = predictive(rng_predict, X, None)
                accuracy += [( (expit(predictions['obs'].mean(axis=0)) > 0.5).astype(int) == list_of_hidden_states).mean()]
            
            glob_intercepts.append(np.array(intercepts))
            glob_slopes.append(np.array(slopes))
            
            axs[0].plot(np.array(intercepts), '-o', label=source_type)
            axs[0].set_ylim(-0.9, .1)
            axs[0].set_title("Intercepts")
            axs[1].plot(np.array(slopes), '-o')
            axs[1].set_ylim(-1.5, 2.3)
            axs[1].set_title("Slopes")
            axs[2].plot(np.array(accuracy), '-o')
            axs[2].set_ylim(0.4, 1.1)
            axs[2].set_title("Accuracy")
            
            for source_type in ['helpful', 'random', 'opposite', 'bluebias']:
                axs_lrs[0].hist(np.log10(learning_rates_a[source_type]))
                axs_lrs[0].set_title("Log10 - Learning Rates a")
                axs_lrs[1].hist(np.log10(learning_rates_b[source_type]))
                axs_lrs[1].set_title("Log10 - Learning Rates b")
                axs_lrs[2].hist(np.log10(learning_rates_c[source_type]))
                axs_lrs[2].set_title("Log10 - Learning Rates c")

            # except Exception as e:
            #     print(e)
            #     continue
        per_part_mean_intercepts = glob_intercepts
        per_part_mean_slopes = glob_slopes
        
        # Log the intercepts and slopes for each source_type
        for i, source_type in enumerate(['helpful', 'opposite', 'bluebias', 'random']):
            logging.info(f"Source Type: {source_type}")
            logging.info(f"Intercepts: {per_part_mean_intercepts[i]}")
            logging.info(f"Slopes: {per_part_mean_slopes[i]}")

        axs[0].legend()
        plt.tight_layout()
        plt.show()
        plt.close()

    elif MODE == "individual":

        ## block halve analysis
        fig, axs = plt.subplots(3, figsize=(10, 20))
        res_dict = {
            'helpful': {
                1: {
                    'intercepts': [],
                    'slopes': []
                },
                2: {
                    'intercepts': [],
                    'slopes': []
                }
            },
            'random': {
                1: {
                    'intercepts': [],
                    'slopes': []
                },
                2: {
                    'intercepts': [],
                    'slopes': []
                }
            },
            'opposite': {
                1: {
                    'intercepts': [],
                    'slopes': []
                },
                2: {
                    'intercepts': [],
                    'slopes': []
                }
            },
            'bluebias': {
                1: {
                    'intercepts': [],
                    'slopes': []
                },
                2: {
                    'intercepts': [],
                    'slopes': []
                }
            }
        }
        for source_type in ['helpful', 'random', 'opposite', 'bluebias']:
            
            for block_half, g_df_half in glob_df.groupby(by='block_half'):
                g_df_filt = g_df_half[
                        (g_df_half['source_type'] == source_type) & \
                                (g_df_half['condition'] == CONDITION)]
                # breakpoint()
                for completion_code, g_df in tqdm(g_df_filt.groupby(by='completion_code')):
                    
                    this_df = g_df[
                        (g_df['source_type'] == source_type) & \
                            (g_df['block_stage'] != 'station_only') & \
                                (g_df['condition'] == CONDITION)]

                    if CONDITION == 'With':
                        list_of_blue_endorsements = this_df['X_F'] - 2.5
                    elif CONDITION == 'Without':
                        list_of_blue_endorsements = this_df['X_I'] - 2.5
                    list_of_hidden_states = ((this_df['ground_truth'] + 1) / 2).to_numpy()
                    list_of_confidences = this_df['posterior_blue_I']
                        
                    # Add a constant term for the intercept
                    # features = np.column_stack((np.arange(len(list_of_confidences)), list_of_blue_endorsements))
                    features = np.array(list_of_blue_endorsements)
                    features_with_intercept = sm.add_constant(features)
                    probabilities = list_of_confidences

                    def logistic_regression_prob_model(X, y_log_prob=None):
                        # Priors for the coefficients (Cauchy priors)
                        intercept = sample('intercept', dist.Cauchy(0.,2.5))
                        beta = sample('beta', dist.Cauchy(0., 10))

                        # Linear model: logits = intercept + X @ beta
                        logits = intercept + jnp.dot(X, beta)
                        # prob = expit(logits)
                            
                        with plate('data', X.shape[0]):
                            sample('obs', dist.Normal(logits, 1.6*2.5 * 6.5), obs=y_log_prob)
                            # sample('obs', dist.Beta(5 * prob, 5 * (1 - prob)), obs=y_prob)
                            
                    rng_next, rng_nuts, rng_predict = jax.random.split(rng_next, 3)
                    # Set up the NUTS sampler
                    nuts_kernel = NUTS(logistic_regression_prob_model)
                    # nuts_kernel = NUTS(bayesian_beta_regression)

                    # Run MCMC to sample from the posterior
                    y_prob = jnp.array(probabilities)
                    eps = 1e-2
                    y_prob = jnp.clip(y_prob, eps, 1 - eps)
                    y_log_prob = jnp.log(y_prob / (1 - y_prob))
                    
                    X = jnp.array(features)
                        
                    mcmc = MCMC(nuts_kernel, num_warmup=100, num_samples=100)
                    mcmc.run(rng_nuts, X=X, y_log_prob=y_log_prob)

                    # Get the posterior samples
                    posterior_samples = mcmc.get_samples()

                    # Summary of the posterior
                    mcmc.print_summary()

                    res_dict[source_type][block_half]['intercepts'].append(posterior_samples['intercept'].mean())
                    res_dict[source_type][block_half]['slopes'].append(posterior_samples['beta'].mean())
                    # Binarize predictions and compare with ground truth
                    predictive = Predictive(logistic_regression_prob_model, posterior_samples, return_sites=['obs'])
                    predictions = predictive(rng_predict, X, None)

        with open(f'res_dict_{SLUG}.pkl', 'wb') as f:
            pickle.dump(res_dict, f)