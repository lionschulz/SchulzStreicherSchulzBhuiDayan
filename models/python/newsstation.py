import logging

import numpy as np
import gymnasium as gym
from functools import partial

from enum import Enum


class WinningPolicy(Enum):
    BLUE=0,
    GREEN=1

class NewsStation(gym.Env):

    def __init__(self, N=5, bI=.5, gI=.5, **kwargs):
        super().__init__(**kwargs)
        self._N = N
        self._bI = bI
        self._gI = gI
        logging.debug("Initializing NewsStation")
        self.reset()
        logging.debug("Winning policy determined")
        logging.debug(f"{self}")


    def reset(self):
        self._winning_policy = np.random.choice([WinningPolicy.BLUE, WinningPolicy.GREEN])
        self._endorsement_distribution_blue = partial(np.random.binomial, self._N, self._bI)
        self._endorsement_distribution_green = partial(np.random.binomial, self._N, 1 - self._gI)
        self._use_endorsement_distribution = self._endorsement_distribution_blue if self._winning_policy == WinningPolicy.BLUE else self._endorsement_distribution_green
        return self._use_endorsement_distribution()

    def step(self, action):
        numeric_value = 0 if self._winning_policy == WinningPolicy.BLUE else 1
        self.reset()
        # return self._use_endorsement_distribution(), 2 * -np.abs(action - numeric_value) + 1, False, False, {}
        return self._use_endorsement_distribution(), -2 * numeric_value + 1, False, False, {}
        

    def __str__(self) -> str:
        news_station_string = f"""News Station: gI={self._gI}, bI={self._bI}, N={self._N}
        Winning policy: {self._winning_policy}"""     
        return super().__str__() + news_station_string


if __name__ == "__main__":
    ns = NewsStation(5, 0.9, 0.5)
    print(ns)
    print(ns.step(0))