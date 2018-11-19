import pickle
from churn_data import ChurnData, getChurnScores
from survival_model import SurvivalModel
import pandas as pd
import numpy as np
from lifelines import CoxPHFitter
from lifelines.utils import concordance_index
from sklearn.metrics import roc_auc_score, mean_squared_error
from sklearn.model_selection import StratifiedKFold

from multiprocessing import Pool
from functools import partial


class CoxSqrtChurnModel(SurvivalModel):
    RESULT_PATH = '../../results/churn/cox_regression_sqrt/'

    def __init__(self, penalizer=2100, include_recency=False):
        super().__init__(include_recency=include_recency)
        self.cf = CoxPHFitter(penalizer=penalizer)

    def transformTargets(self, targets):
        return np.sqrt(targets)

    def reverseTransformTargets(self, targets):
        return targets**2

