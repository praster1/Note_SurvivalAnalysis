import pickle
import os.path

import numpy as np
import pandas as pd

from keras.models import Sequential, Model
from keras.layers import Dense, Dropout, Activation, Input, Masking, concatenate, Embedding, RepeatVector, Reshape
from keras.layers.recurrent import LSTM
from keras.callbacks import Callback, LambdaCallback, TensorBoard, ReduceLROnPlateau, EarlyStopping, ModelCheckpoint
from keras.optimizers import Adam, RMSprop
from keras.constraints import unit_norm, max_norm
from keras import regularizers
from keras import backend as K

from sklearn.model_selection import train_test_split, StratifiedShuffleSplit
from sklearn.metrics import mean_squared_error, accuracy_score, recall_score, roc_auc_score
from lifelines.utils import concordance_index
from sklearn.gaussian_process.kernels import Matern
from bayes_opt import BayesianOptimization

import sys
sys.path.insert(0, '../rmtpp')
sys.path.insert(0, '../utils')
from rmtpp_data import *

from plot_format import *
from seaborn import apionly as sns

seed = 42
np.random.seed(seed)

RESULT_PATH = '../../results/rnn/bayes_opt/'

class Rnn:

    def __init__(self, name, run, hidden_neurons=10, n_sessions=50):
        self.predict_sequence = False
        self.hidden_neurons = hidden_neurons
        self.n_sessions = n_sessions
        self.data = RmtppData.instance()
        self.set_x_y(n_sessions=n_sessions)
        self.set_model()
        self.name = name
        self.run = run
        self.best_model_cp_file = '../../results/rnn_new_2/{:02d}_{}.hdf5'.format(run, name)
        self.best_model_cp = ModelCheckpoint(self.best_model_cp_file, monitor="val_loss",
                                             save_best_only=True, save_weights_only=False)

    def set_x_y(self, min_n_sessions=0, n_sessions=100, preset='deltaNextDays'):
        self.x_train, \
        self.x_test, \
        self.x_train_unscaled, \
        self.x_test_unscaled, \
        self.y_train, \
        self.y_test, \
        self.features, \
        self.targets = self.data.get_xy(min_n_sessions=min_n_sessions, n_sessions=n_sessions, preset=preset, target_sequences=self.predict_sequence)

        if self.predict_sequence:
            self.y_train_churned = self.y_train[:,-1,self.targets.index('churned')].astype('bool')
            self.y_test_churned = self.y_test[:,-1,self.targets.index('churned')].astype('bool')
        else:
            self.y_train_churned = self.y_train[:,self.targets.index('churned')].astype('bool')
            self.y_test_churned = self.y_test[:,self.targets.index('churned')].astype('bool')

        train_train_i, train_val_i = self.train_i, self.test_i = next(StratifiedShuffleSplit(test_size=.2, random_state=42).split(self.x_train, self.y_train_churned))

        temporal_features = ['dayOfMonth', 'dayOfWeek', 'hourOfDay', 'deltaPrevDays', 'startUserTimeDays', 'sessionLengthSec']
        # temporal_features = ['deltaPrevDays', 'startUserTimeDays', 'sessionLengthSec']
        # device_index = self.features.index('device')
        self.device_indices = [i for i,f in enumerate(self.features) if f.startswith('device')]
        self.temporal_indices = list(map(self.features.index, temporal_features))
        self.behav_indices = list(map(self.features.index, set(self.features) - set(temporal_features + ['device'])))
        # used_feature_indices = [self.features.index('deltaPrevDays')]
        # used_feature_indices = device_indices + temporal_indices
        used_feature_indices = list(range(len(self.features)))

        self.x_train = self.x_train[:,:,used_feature_indices]
        self.x_test = self.x_test[:,:,used_feature_indices]
        self.x_train_train = self.x_train[train_train_i]
        self.x_train_val = self.x_train[train_val_i]
        self.x_train_train_unscaled = self.x_train_unscaled[train_train_i]
        self.x_train_val_unscaled = self.x_train_unscaled[train_val_i]

        self.y_train_train = self.y_train.T[1].T.astype('float32')[train_train_i]
        self.y_train_val = self.y_train.T[1].T.astype('float32')[train_val_i]

        self.y_train_train_churned = self.y_train_churned[train_train_i]
        self.y_train_val_churned = self.y_train_churned[train_val_i]

        self.x_train_ret = self.x_train[~self.y_train_churned]
        self.x_train_train_ret = self.x_train_train[~self.y_train_train_churned]
        self.x_train_val_ret = self.x_train_val[~self.y_train_val_churned]
        self.y_train_ret = self.y_train.T[1].T.astype('float32')[~self.y_train_churned]
        self.y_train_train_ret = self.y_train_train[~self.y_train_train_churned]
        self.y_train_val_ret = self.y_train_val[~self.y_train_val_churned]

        self.y_train = self.y_train.T[1].T.astype('float32')
        self.y_test = self.y_test.T[1].T.astype('float32')

        if self.predict_sequence:
            self.y_train_train = self.y_train_train.reshape(self.y_train_train.shape+(1,))
            self.y_train_val = self.y_train_val.reshape(self.y_train_val.shape+(1,))
            self.y_train_train_ret = self.y_train_train_ret.reshape(self.y_train_train_ret.shape+(1,))
            self.y_train_val_ret = self.y_train_val_ret.reshape(self.y_train_val_ret.shape+(1,))
            self.y_train = self.y_train.reshape(self.y_train.shape+(1,))
            self.y_test = self.y_test.reshape(self.y_test.shape+(1,))
            self.y_train_ret = self.y_train_ret.reshape(self.y_train_ret.shape+(1,))

    def load_best_weights(self):
        self.model.load_weights(self.best_model_cp_file)

    def set_model(self, lr=0.01):
        self.lr = lr
        len_seq = self.x_train.shape[1]
        n_devices = self.x_train[:,:,self.device_indices].max() + 1
        n_feat = self.x_train.shape[2]
        len_temporal = len(self.temporal_indices)
        len_behav = len(self.behav_indices)
        # len_temporal = self.x_train_temporal.shape[2]
        # len_behav = self.x_train_behav.shape[2]

        lstm_neurons = self.hidden_neurons

        inputs = Input(shape=(len_seq, n_feat))
        merge_inputs = Masking(mask_value=0.)(inputs)

        # lstm_output = LSTM(lstm_neurons, return_sequences=self.predict_sequence, kernel_constraint=max_norm())(merge_inputs)
        # lstm_output = LSTM(lstm_neurons, return_sequences=self.predict_sequence)(merge_inputs)
        lstm_output = LSTM(lstm_neurons, return_sequences=self.predict_sequence, kernel_regularizer=regularizers.l2(0.005))(merge_inputs)


        # predictions = Dense(1, activation='relu', name='predictions', kernel_constraint=max_norm())(lstm_output)
        # predictions = Dense(1, activation='relu', name='predictions')(lstm_output)
        predictions = Dense(1, activation='relu', name='predictions', kernel_regularizer=regularizers.l2(0.005))(lstm_output)

        model = Model(inputs, outputs=predictions)

        # model.compile(loss=self.weighted_mean_squared_error, optimizer=RMSprop(lr=lr))
        model.compile(loss='mse', optimizer=RMSprop(lr=lr))
        # model.compile(loss='mse', optimizer=Adam(lr=lr))

        self.model = model
        return model

    def weighted_mean_squared_error(self, y_true, y_pred):
        mask = K.cast(K.not_equal(y_true, 0), 'float32')
        n = 1/K.sum(mask, 1)
        n = K.tile(n, (K.shape(n)[1], self.n_sessions))
        n = K.batch_flatten(mask) * n

        sq_errs = K.batch_flatten(K.square(y_pred - y_true))
        w_sq_errs = sq_errs * n
        sum_w_sq_errs = K.sum(w_sq_errs)
        return sum_w_sq_errs / K.sum(n)


    def get_scores(self, dataset='val'):
        if dataset=='val':
            x = self.x_train_val
            y_0 = self.y_train_val
            churned = self.y_train_val_churned
            unscaled = self.x_train_val_unscaled
        elif dataset=='train':
            x = self.x_train_train
            y_0 = self.y_train_train
            churned = self.y_train_train_churned
            unscaled = self.x_train_train_unscaled
        else:
            x = self.x_test
            y_0 = self.y_test
            churned = self.y_test_churned
            unscaled = self.x_test_unscaled

        pred_0 = self.model.predict(x)
        # pred_0 = np.array([58.384029]*self.y_test.shape[0])
        # pred_0 = 365 -  self.x_test_unscaled[:,-1,13] + 25
        # pred_0 = np.load('../../results/evaluation/rmtpp_abs/predictions_days.npy') - 47.04

        # pred_0 = self.model.predict([x[:,:,self.device_indices[0]], x[:,:,self.temporal_indices], x[:,:,self.behav_indices]])

        if self.predict_sequence:
            mask = y_0 != 0
            churned_mask = mask[~churned]
            pred_last = pred_0[:,-1].ravel()
            y_last = y_0[:,-1].ravel()
        else:
            pred_last = pred_0.ravel()
            y_last = y_0.ravel()
        # return pred_0, y_0, churned, churned_mask, mask

        rmse_days = np.sqrt(mean_squared_error(pred_last[~churned], y_last[~churned]))

        if self.predict_sequence:
            rmse_days_all = np.sqrt(mean_squared_error(pred_0[~churned][churned_mask].ravel(), y_0[~churned][churned_mask].ravel()))
        else:
            rmse_days_all = 0

        rtd_ind = self.features.index('startUserTimeDays')
        ret_time_days_pred = unscaled[:,-1,rtd_ind] + pred_last
        ret_time_days_true = unscaled[:,-1,rtd_ind] + y_last

        churned_pred = ret_time_days_pred >= churn_days
        churned_true = ret_time_days_true >= churn_days

        churn_acc = accuracy_score(churned_true, churned_pred)
        churn_recall = recall_score(churned_true, churned_pred)
        churn_auc = roc_auc_score(churned_true, pred_last)

        concordance = concordance_index(y_last, pred_last, ~churned)

        return {'rmse_days': rmse_days,
                'rmse_days_all': rmse_days_all,
                'churn_acc': churn_acc,
                'churn_auc': churn_auc,
                'churn_recall': churn_recall,
                'concordance': concordance}


    def fit_model(self, initial_epoch=0):
        log_file = '{:02d}_{}_lr{}_inp{}_nsess{}_hiddenNr{}'.format(self.run, self.name, self.lr,self.x_train.shape[2], self.n_sessions, self.hidden_neurons)
        # self.model.fit([self.x_train_devices, self.x_train_temporal, self.x_train_behav], self.y_train, batch_size=1000, epochs=500, validation_split=0.2, verbose=0, initial_epoch=initial_epoch
        # self.model.fit(self.x_train, self.y_train, batch_size=1000, epochs=500, validation_split=0.2, verbose=0, initial_epoch=initial_epoch
        # self.model.fit(self.x_train_train_ret, self.y_train_train_ret, batch_size=1000, epochs=1000, validation_split=0.2, verbose=0, initial_epoch=initial_epoch
        self.model.fit(self.x_train_ret, self.y_train_ret, batch_size=1000, epochs=2000, validation_split=0.2, verbose=0, initial_epoch=initial_epoch
              , callbacks=[
                TensorBoard(log_dir='../../logs/rnn_new_2/{}'.format(log_file), histogram_freq=100)
                , EarlyStopping(monitor='val_loss', min_delta=0, patience=100, verbose=1, mode='auto')
                , self.best_model_cp
                ]
             )



def runBayesOpt():
    RESULT_PATH = '../../results/rnn/bayes_opt/'

    bounds = {'hidden_neurons': (1, 100), 'n_sessions': (1,150)}
    n_iter = 20

    bOpt = BayesianOptimization(_evaluatePerformance, bounds)

    bOpt.maximize(init_points=8, n_iter=n_iter, acq='ucb', kappa=5, kernel=Matern())

    with open(RESULT_PATH+'bayes_opt_rnn_5.pkl', 'wb') as handle:
        pickle.dump(bOpt, handle, protocol=pickle.HIGHEST_PROTOCOL)

    return bOpt

def _evaluatePerformance(hidden_neurons, n_sessions):
    # def __init__(self, name, run, hidden_neurons=32, n_sessions=100):
    K.clear_session()
    hidden_neurons = np.floor(hidden_neurons).astype('int')
    n_sessions = np.floor(n_sessions).astype('int')
    model = Rnn('bayes_opt', 85, hidden_neurons, n_sessions)
    model.fit_model()
    model.load_best_weights()
    pred = model.model.predict(model.x_train_val_ret)
    mse = mean_squared_error(pred, model.y_train_val_ret)
    return -mse


predPeriod = {
    'start': pd.Timestamp('2016-02-01'),
    'end': pd.Timestamp('2016-06-01')
}
obsPeriod = {
    'start': pd.Timestamp('2015-02-01'),
    'end': pd.Timestamp('2016-02-01')
}
predPeriodHours = (predPeriod['end'] - predPeriod['start']) / np.timedelta64(1, 'h')
hours_year = np.timedelta64(pd.datetime(2017,2,1) - pd.datetime(2016,2,1)) / np.timedelta64(1,'h')
churn_days = (predPeriod['end'] - obsPeriod['start']) / np.timedelta64(24, 'h')

def showResidPlot_short_date(model, y_pred, width=1, height=None):
    startUserTimeDaysCol = model.features.index('startUserTimeDays')
    df = pd.DataFrame()
    df['predicted (days)'] = y_pred
    df['actual (days)'] = model.y_test
    df['daysInObs'] = model.y_test_startTime
    df['date'] = df['daysInObs'] * np.timedelta64(24,'h') + obsPeriod['start']
    df['residual (days)'] = df['predicted (days)'] - df['actual (days)']

    grid = sns.JointGrid('daysInObs', 'residual (days)', data=df, size=figsize(.5,.5)[0], xlim=(0,3000), ylim=(-110,110))
    grid = grid.plot_marginals(sns.distplot, kde=False, color='k')#, shade=True)
    # grid = grid.plot_joint(sns.kdeplot, shade=True, n_levels=12, cmap='Blues', shade_lowest=False, cut=0)
    grid = grid.plot_joint(plt.scatter, alpha=.1, s=6, lw=0)
    # grid = grid.plot_joint(sns.kdeplot, shade=True, n_levels=15, cmap='Blues', shade_lowest=False, cut=0)
    # grid = grid.plot(df.hoursInPred, df['residual (days)'])
    # grid.ax_joint.clear()
    # grid.ax_joint.scatter(df.hoursInPred, df['residual (days)'])
    grid.ax_joint.clear()

    retUnc = grid.ax_joint.scatter(df['daysInObs'], df['residual (days)'], alpha=.1, s=6, lw=0, color='C0', label='Ret. user (uncens.)')

#     obs_cens = ~model.data.split_val_df.observed & ~model.data.split_val_df.churnedFull.astype('bool')
#     retCens = grid.ax_joint.scatter(df.loc[obs_cens, 'hoursInPred'], df.loc[obs_cens, 'residual (days)'], alpha=.1, s=6, lw=0, color='C4', label='Ret. user (cens.)')

    xDates = [pd.datetime(2016,i,1) for i in [2,4,6]]
    xDatesHours = [(d - obsPeriod['start']).to_timedelta64()/np.timedelta64(24,'h') for d in xDates]
    xDatesStr = [d.strftime('%Y-%m') for d in xDates]
    grid.ax_joint.set_xticks(xDatesHours)
    grid.ax_joint.set_xticklabels(xDatesStr)
    grid.ax_joint.set_xlabel('actual return date')
    grid.ax_joint.set_ylabel('residual (days)')
    # grid.ax_joint.legend(handles=[retUnc], loc=1, labelspacing=0.02, handlelength=0.5)
    # grid = grid.plot_joint(plt.scatter)#, shade=True, n_levels=10, cmap='Blues', shade_lowest=False, cut=0)

    grid.ax_joint.set_ylim((-110,110))
    plt.show()


def showResidPlot_short_days(model, y_pred, width=1, height=None):
    startUserTimeDaysCol = model.features.index('startUserTimeDays')
    df = pd.DataFrame()
    df['predicted (days)'] = y_pred
    df['actual (days)'] = model.y_test
    df['daysInObs'] = model.y_test_startTime
    df['date'] = df['daysInObs'] * np.timedelta64(24,'h') + obsPeriod['start']
    df['residual (days)'] = df['predicted (days)'] - df['actual (days)']

    grid = sns.JointGrid('actual (days)', 'residual (days)', data=df, size=figsize(.5,.5)[0], xlim=(0,3000), ylim=(-110,110))
    grid = grid.plot_marginals(sns.distplot, kde=False, color='k')#, shade=True)
    # grid = grid.plot_joint(sns.kdeplot, shade=True, n_levels=12, cmap='Blues', shade_lowest=False, cut=0)
    grid = grid.plot_joint(plt.scatter, alpha=.1, s=6, lw=0)
    # grid = grid.plot_joint(sns.kdeplot, shade=True, n_levels=15, cmap='Blues', shade_lowest=False, cut=0)
    # grid = grid.plot(df.hoursInPred, df['residual (days)'])
    # grid.ax_joint.clear()
    # grid.ax_joint.scatter(df.hoursInPred, df['residual (days)'])
    grid.ax_joint.clear()

    retUnc = grid.ax_joint.scatter(df['actual (days)'], df['residual (days)'], alpha=.1, s=6, lw=0, color='C0', label='Ret. user (uncens.)')

#     obs_cens = ~model.data.split_val_df.observed & ~model.data.split_val_df.churnedFull.astype('bool')
#     retCens = grid.ax_joint.scatter(df.loc[obs_cens, 'hoursInPred'], df.loc[obs_cens, 'residual (days)'], alpha=.1, s=6, lw=0, color='C4', label='Ret. user (cens.)')

    # xDates = [pd.datetime(2016,i,1) for i in [2,4,6]]
    # xDatesHours = [(d - obsPeriod['start']).to_timedelta64()/np.timedelta64(24,'h') for d in xDates]
    # xDatesStr = [d.strftime('%Y-%m') for d in xDates]
    # grid.ax_joint.set_xticks(xDatesHours)
    # grid.ax_joint.set_xticklabels(xDatesStr)
    grid.ax_joint.set_xlabel('actual return time (days)')
    grid.ax_joint.set_ylabel('residual (days)')
    grid.ax_joint.set_ylim((-110,110))
    # grid.ax_joint.legend(handles=[retUnc], loc=1, labelspacing=0.02, handlelength=0.5)
    # grid = grid.plot_joint(plt.scatter)#, shade=True, n_levels=10, cmap='Blues', shade_lowest=False, cut=0)

    plt.show()


