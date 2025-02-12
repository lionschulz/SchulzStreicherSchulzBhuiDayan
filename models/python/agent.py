import numpy as np
from scipy.special import softmax, expit
from newsstation import NewsStation

class Agent():
    pass


class RescorlaWagnerValue():
    def __init__(self, alpha):
        self.alpha = alpha
        self.value = 0

    def learn(self, prediction_error):
        self.value += self.alpha * prediction_error

    def predict(self):
        return self.value
    
class RescorlaWagnerAgent(Agent):
    def __init__(self, alpha, n_values_to_track=2):
        self._values_to_track = [RescorlaWagnerValue(alpha) for _ in range(n_values_to_track + 1)]

    def update(self, x, y_actual):

        prediction = self._values_to_track[x].predict()
        prediction_error = y_actual - prediction
        self._values_to_track[x].learn(prediction_error)
        # print([value.predict() for value in self._values_to_track])

    def predict(self, x):
        return self._values_to_track[x].predict()
    
    def confidence(self):
        return [value.predict() for value in self._values_to_track]

    def act(self, observation: int = 0):
        psychometric_function = softmax([value.predict() for value in self._values_to_track])
        if psychometric_function[observation] > 0.5:
            return 1
        else:
            return 0
        

class QuadraticRescorlaWagnerAgent(Agent):
    def __init__(self, learning_rate=0.01, degree=1):
        """
        Initialize the Rescorla-Wagner agent with quadratic learning capabilities.

        Parameters:
        - learning_rate: The rate at which the agent learns (default is 0.01).
        """
        self.learning_rate = learning_rate
        self._degree = degree

        self._parameters = np.random.normal(loc=0, scale=0.01, size=degree + 1) # + Bias term

    def predict(self, x):
        """
        Predict the output for a given input x based on the current weights.

        Parameters:
        - x: The input value.

        Returns:
        - The predicted output.
        """
        # return self.a*x**2 + self.b * x + self.c + self.d * x**3
        return np.dot([x**i for i in range(self._degree + 1)], self._parameters)

    def update(self, x, y_actual):
        """
        Update the agent's weights based on the observed input-output pair (x, y_actual).

        Parameters:
        - x: The input value.
        - y_actual: The actual output value observed.
        """
        # Calculate the prediction
        y_pred = self.predict(x)
        # y_pred = 1 if y_pred > 0.0 else -1
        # y_pred = expit(y_pred)

        # Compute prediction error
        error = y_actual - y_pred # diff reward and q value 

        print(error)

        # Update weights using Rescorla-Wagner rule
        self._parameters += self.learning_rate * error * np.array([x**i for i in range(self._degree + 1)])

        return error

        # breakpoint()

    def train(self, data, epochs=100):
        """
        Train the agent on a dataset of input-output pairs.

        Parameters:
        - data: A list of tuples [(x1, y1), (x2, y2), ...] representing the training data.
        - epochs: The number of training epochs to run (default is 100).
        """
        for epoch in range(epochs):
            for x, y in data:
                self.update(x, y)

    def confidence(self, x): 
        return np.exp(self.predict(x)) / (1 + np.exp(self.predict(x)))
    
    def __repr__(self):
        """
        Representation of the current state of the model.

        Returns:
        - A string representing the current quadratic function approximation.
        """
        # return f"f(x) = {self.a:.4f}x^2 + {self.b:.4f}x + {self.c:.4f} + {self.d:.4f}x^3"
        return f"f(x) = {' + '.join([f'{self._parameters[i]:.4f}x^{i}' for i in range(self._degree + 1)])}"
    
def test_quadratic_agent():
    N_DATAPOINTS = 50
    N_EPOCHS = 20

    x_grid = np.linspace(-3, 3)
    # y_true = 2 * x_grid**2 - 3 * x_grid + 5
    ## a biased sigmoid function
    y_true = expit(x_grid + 1)

    # Create a noisy dataset
    np.random.seed(0)
    noise = np.random.normal(0, .01, N_DATAPOINTS)
    y_noisy = y_true + noise
    
    data = list(zip(x_grid, y_noisy))

    # Initialize the agent
    agent = QuadraticRescorlaWagnerAgent(learning_rate=0.01)

    import matplotlib.pyplot as plt
    
    plt.scatter(x_grid, y_noisy, label="Noisy data")

    # Train the agent on the dataset
    for x in range(N_EPOCHS):
        agent.train(data, epochs=1)
    
        # Check the final weights
        print(agent)

        plt.plot(x_grid, agent.predict(x_grid), label="True function", color='green', linewidth=2, alpha=x / N_EPOCHS)
    
    plt.show()

def test_quadratic_agent_dummy_newsstation():
    N_DATAPOINTS = 50
    N_STEPS = 50_000
    LEARNING_RATE = 0.0001
    DEGREE = 3

    import matplotlib.pyplot as plt

    fig, (ax1, ax2, ax3) = plt.subplots(3)

    for agent, news_station, label in zip(
        [QuadraticRescorlaWagnerAgent(learning_rate=LEARNING_RATE, degree=DEGREE), 
         QuadraticRescorlaWagnerAgent(learning_rate=LEARNING_RATE, degree=DEGREE),
         QuadraticRescorlaWagnerAgent(learning_rate=LEARNING_RATE, degree=DEGREE),
         QuadraticRescorlaWagnerAgent(learning_rate=LEARNING_RATE, degree=DEGREE)], 
        [NewsStation(5, 0.75, 0.75), 
         NewsStation(5, 0.25, 0.25),
         NewsStation(5, 0.9, 0.5),
         NewsStation(5, 0.5, 0.5)],
        ['informative', 
         'inverse', 
         'blue_biased',
         'random']):

        obs_prev = news_station.reset()
        datapoints = []
        values = []
        pred_correct = []
        for step in range(N_STEPS):
            # breakpoint()
            action = 1 if agent.predict(obs_prev) > 0. else -1
            obs, reward, *__ = news_station.step(action)
            datapoints += [(obs_prev, reward + np.random.normal(0, 0.01))]
            values += [agent.confidence(0)]
            print(action, agent.predict(obs_prev), obs_prev, reward)
            agent.update(obs_prev, reward)
            print(agent)
            obs_prev = obs
            pred_correct += [action == reward]

        ax1.plot(np.arange(6), [agent.predict(x) for x in np.arange(6)], label=label)
        ax2.plot(np.arange(6), ([agent.confidence(x) for x in np.arange(6)]), label=label)
        ax1.scatter(*zip(*datapoints), label=label, alpha = .2)
        # plot a rolling accuracy
        ax3.plot(np.convolve(pred_correct, np.ones(1000)/1000, mode='valid'), label=label)
        # plt.plot(values, label=label) 
        # breakpoint()
    plt.legend()
    plt.show()


if __name__ == "__main__":
    # test_quadratic_agent()
    test_quadratic_agent_dummy_newsstation()