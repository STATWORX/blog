import numpy as np
import pandas as pd
from datetime import date
from matplotlib import pyplot as plt
from keras.models import Sequential
from keras.layers import InputLayer, Reshape, Conv1D, MaxPool1D, Flatten, Dense, LSTM
from keras.callbacks import EarlyStopping, ModelCheckpoint


def prepare_data(target, window_X, window_y):
    """ Data preprocessing for multistep forecast """
    X, y = [], []
    start_X = 0
    end_X = start_X + window_X
    start_y = end_X
    end_y = start_y + window_y
    for _ in range(len(target)):
        if end_y < len(target):
            X.append(target[start_X:end_X])
            y.append(target[start_y:end_y])
        start_X += 1
        end_X = start_X + window_X
        start_y += 1
        end_y = start_y + window_y
    X = np.array(X)
    y = np.array(y)
    return np.array(X), np.array(y)


def fit_model(type, X_train, y_train, X_test, y_test, batch_size, epochs):
    """ Training function for network """
    # Model input
    model = Sequential()
    model.add(InputLayer(input_shape=(X_train.shape[1], )))

    if type == 'mlp':
        model.add(Reshape(target_shape=(X_train.shape[1], )))
        model.add(Dense(units=64, activation='relu'))

    if type == 'cnn':
        model.add(Reshape(target_shape=(X_train.shape[1], 1)))
        model.add(Conv1D(filters=64, kernel_size=4, activation='relu'))
        model.add(MaxPool1D())
        model.add(Flatten())

    if type == 'lstm':
        model.add(Reshape(target_shape=(X_train.shape[1], 1)))
        model.add(LSTM(units=64, return_sequences=False))

    # Output layer
    model.add(Dense(units=64, activation='relu'))
    model.add(Dense(units=y_train.shape[1], activation='sigmoid'))

    # Compile
    model.compile(optimizer='adam', loss='mse')

    # Callbacks
    early_stopping = EarlyStopping(monitor='val_loss', patience=10)
    model_checkpoint = ModelCheckpoint(filepath='model.h5', save_best_only=True)
    callbacks = [early_stopping, model_checkpoint]

    # Fit model
    model.fit(x=X_train, y=y_train, validation_data=(X_test, y_test),
              batch_size=batch_size, epochs=epochs, callbacks=callbacks, verbose=2)

    # Load best model
    model.load_weights('model.h5')

    # Return
    return model


# Define windows
window_X = 12
window_y = 6

# Load data
data = pd.read_csv('data.csv', sep=',')
data = data.set_index(keys=pd.to_datetime(data['month']), drop=True).drop('month', axis=1)

# Scale data
data['y'] = data['y'] / 100.

# Prepare tensors
X, y = prepare_data(target=data['y'].values, window_X=window_X, window_y=window_y)

# Training and test
train = 100
X_train = X[:train]
y_train = y[:train]
X_valid = X[train:]
y_valid = y[train:]

# Train models
models = ['mlp', 'cnn', 'lstm']

# Test data
X_test = data['y'].values[-window_X:].reshape(1, -1)

# Predictions
preds = pd.DataFrame({'mlp': [np.nan]*6, 'cnn': [np.nan]*6, 'lstm': [np.nan]*6})
preds = preds.set_index(pd.DatetimeIndex(start=date(2018, 11, 1), end=date(2019, 4, 1), freq='MS'))

# Fit models and plot
for mod in models:

    # Train models
    model = fit_model(type=mod, X_train=X_train, y_train=y_train, X_test=X_valid, y_test=y_valid, batch_size=16, epochs=1000)

    # Predict
    p = model.predict(x=X_test)

    # Fill
    preds[mod] = p[0]

# Plot
idx = pd.DatetimeIndex(start=date(2004, 1, 1), end=date(2019, 4, 1), freq='MS')
data = data.reindex(idx)
plt.plot(data.iloc[-36:, ]['y'], label='Google')

# Plot
plt.plot(preds['mlp'], label='MLP')
plt.plot(preds['cnn'], label='CNN')
plt.plot(preds['lstm'], label='LSTM')
plt.legend()
