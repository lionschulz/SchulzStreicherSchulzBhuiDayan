from dataclasses import dataclass
from typing import List

from multiprocessing import Pool
import matplotlib.pyplot as plt
import numpy as np

from scipy.special import expit

from newsstation import NewsStation
from agent import RescorlaWagnerAgent

def simulate_newsstation(agent, news_station, n_steps=10) -> List[float]:
    rewards = []

    obs = news_station.reset()

    for k in range(MAX_ITER := n_steps):
        # breakpoint()

        action = agent.act(obs)
        # print(action)

        obs, reward, *__ = news_station.step(action)

        prediction_error = reward - agent._values_to_track[obs].predict()
        print(obs, action, reward, prediction_error)
        agent.learn(obs, prediction_error)
        rewards += [reward]

    # print(rewards)
    # axs.plot(agent.confidence())
    # plt.plot(np.cumsum(rewards))
    return rewards

@dataclass
class SimSettings:
    n_steps: int
    learning_rate: float
    n_experiment_repeats: int

def run_one_repetition(settings: SimSettings) -> None:

    helpful_newsstation = NewsStation(5, 0.75, 0.75)
    unhelpful_newsstation = NewsStation(5, 0.25, 0.25)
    neutral_newsstation = NewsStation(5, 0.5, 0.5)
    blue_biased_newsstation = NewsStation(5, 0.9, 0.5)


    agent1 = RescorlaWagnerAgent(alpha=settings.learning_rate, n_values_to_track=5)

    re1 = simulate_newsstation(agent1, helpful_newsstation, n_steps=settings.n_steps)

    agent2 = RescorlaWagnerAgent(alpha=settings.learning_rate, n_values_to_track=5)

    re2 = simulate_newsstation(agent2, unhelpful_newsstation, n_steps=settings.n_steps)

    agent3 = RescorlaWagnerAgent(alpha=settings.learning_rate, n_values_to_track=5)

    re3 = simulate_newsstation(agent3, neutral_newsstation, n_steps=settings.n_steps)

    agent4 = RescorlaWagnerAgent(alpha=settings.learning_rate, n_values_to_track=5)

    re4 = simulate_newsstation(agent4, blue_biased_newsstation, n_steps=settings.n_steps)

    return {
        'helpful': (re1, agent1), 
        'unhelpful': (re2, agent2),
        'neutral': (re3, agent3),
        'blue_biased': (re4, agent4)}

def run_simulation(settings: SimSettings) -> None:
    iterations = []
    with Pool() as pool:
        results = pool.map(run_one_repetition, [settings] * settings.n_experiment_repeats)
    
    for it, result in enumerate(results):
        iterations.append(result)

    return iterations


def simulate_general_curve():

    fig, (ax1, ax2) = plt.subplots(2)

    sim_settings = SimSettings(
        n_steps=2_000, 
        learning_rate=0.3, 
        n_experiment_repeats=100
    )

    res = run_simulation(sim_settings)

    reward_curve = np.array([np.cumsum(it_re['helpful'][0]) for it_re in res])
    confidence_curve_helpful = np.array([it_re['helpful'][1].confidence() for it_re in res])
    confidence_curve_unhelpful = np.array([it_re['unhelpful'][1].confidence() for it_re in res])
    confidence_curve_neutral = np.array([it_re['neutral'][1].confidence() for it_re in res])
    confidence_curve_blue_biased = np.array([it_re['blue_biased'][1].confidence() for it_re in res])

    ax1.plot(np.mean(confidence_curve_helpful, axis=0), label='helpful')
    ax1.plot(np.mean(confidence_curve_unhelpful, axis=0), label='unhelpful')
    ax1.plot(np.mean(confidence_curve_neutral, axis=0), label='neutral')
    ax1.plot(np.mean(confidence_curve_blue_biased, axis=0), label='blue_biased')

    """Fill between to show standard deviation"""

    ax2.plot(np.mean(reward_curve, axis=0), label='helpful')
    ax2.fill_between(
        range(sim_settings.n_steps),
        np.mean(reward_curve, axis=0) - np.std(reward_curve, axis=0),
        np.mean(reward_curve, axis=0) + np.std(reward_curve, axis=0),
        alpha=0.5
    )

    ax1.set_xlim(0, 5)
    ax1.legend()
    ax1.set_xlabel('# blue in report')
    ax1.set_ylabel('Rescorla-Wagner value')
    # axs.set_ylim(0, 1)
    ax2.set_xlabel("step")
    ax2.set_ylabel("cumulative reward")

    plt.show()
    plt.close()
    
    
if __name__ == "__main__":
    import pandas as pd 
    from agent import QuadraticRescorlaWagnerAgent
    df = pd.read_csv('data/df_main_task_wide.csv')

    n_iter = 0
    labels = []
    learning_rates = []
    accuracies = []
    # for learning_rate in 10**np.linspace(-6, 0, 100):
    for learning_rate in [2e-2]:

        accu = []
        completion_code, source_type = ('oovk5u7v', 'helpful')
        group = df.groupby(by=["completion_code", "source_type"]).get_group(('oovk5u7v', 'helpful'))
        agent = QuadraticRescorlaWagnerAgent(learning_rate=learning_rate, degree=2)
        # agent = RescorlaWagnerAgent(alpha=learning_rate, n_values_to_track=5)
        errors = []
        pred_correct = []
        for i, row in group.iterrows():
            confi = agent.predict(row['X_F'])
            action = 1 if confi > 0. else -1
            # print(action, row['ground_truth'])
            # print(row)
            errors += [agent.update(row['X_F'], row['ground_truth'])]
            pred_correct += [action == row['ground_truth']]
            print(agent)
        # plt.plot(np.cumsum(errors))
        print(f"Accuracy: {np.mean(pred_correct)}")
        n_iter += 1
        labels += [row['source_type']]
        if learning_rate > 0:
            fig, axs = plt.subplots(2)
            axs[0].plot(np.arange(6), [agent.confidence(x) for x in np.arange(6)], label=row['source_type'])
            axs[0].legend()
            slice_size = len(pred_correct) // 4
            slices = [pred_correct[i:i+slice_size] for i in range(0, len(pred_correct), slice_size)]
            mean_accuracies = [np.mean(slice) for slice in slices]

            axs[1].plot(mean_accuracies, label=row['source_type'])
            axs[1].legend()
            plt.show()
            plt.close()
    
        learning_rates += [learning_rate]
        accuracies += [np.mean(pred_correct)]

    print(f"best LR: {learning_rates[np.argmax(accuracies)]}")
    print(f"best accuracy: {np.max(accuracies)}")
    breakpoint()
            
    plt.plot(learning_rates, accuracies)
    plt.xscale('log')
    plt.show()
    plt.close()
        # breakpoint()