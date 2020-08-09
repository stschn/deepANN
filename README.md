<!-- # deepANN -->
<h2>Overview deepANN</h2>
Neural Network Toolbox

This R library is currently structured in form of the following functional families:

<b>Dummy</b>
<ul>
  <li><code>dummify()</code> creates dummy variables for non-metric variables.</li>
  <li><code>effectcoding()</code> changes a binary 0/1 encoded variable into a -1/1 encoded variable.</li>
  <li><code>one_hot_encode()</code> builds a one-hot vector in form of a matrix.</li>
  <li><code>one_hot_decode()</code> restores a one-hote encoded matrix to a single vector.</li>
  <li><code>resample.imbalanced()</code> creates dummy rows for an imbalanced data set thru the techniques oversampling, undersampling or SMOTE.</li>
  <li><code>append_rows()</code> appends dummy rows.</li>
  <li><code>remove_columns()</code> removes columns with only one specific value.</li>
</ul>

<b>Outlier</b>
<ul>
  <li><code>outlier()</code> detects outlier thru methods like from Tukey (1977) or maximum likelihood estimation.</li>
  <li><code>outlier.dataset()</code> replaces outliers within a data set.</li>
  <li><code>winsorize()</code> winsorizes a numerical vector.</li>
</ul>

<b>Scaling</b>
<ul>
  <li><code>scale.minmax()</code> scales a numeric vector thru min-max scaling.</li>
  <li><code>scale.zscore()</code> scales a numeric vector thru z-score scaling.</li>
  <li><code>scale.log()</code> scales a numeric vector thru log transformation.</li>
  <li><code>scaling()</code> encapsulates the different types of scaling.</li>
  <li><code>scale.datasets()</code> scales a train and a test data set with a specific scale type.</li>
</ul>

<b>Time Series</b>
<ul>
  <li><code>get.season()</code> delivers corresponding seasons for a given date vector.</li>
  <li><code>lags()</code> builds a lagged data set.</li>
  <li><code>build.stationary()</code> creates a stationary data series thru differencing.</li>
  <li><code>invert_differencing()</code> inverts a differenced data series.</li>
  <li><code>diffinv.simple()</code> inverts a simple-differenced data series.</li>
  <li><code>diff.log()</code> creates a log-differenced data series.</li>
  <li><code>diffinv.log()</code> inverts a log-differenced data series.</li>
  <li><code>diff.percentage()</code> creates a percentage-differenced data series.</li>
  <li><code>diffinv.percentage()</code> inverts a percentage-differenced data series.</li>
  <li><code>period()</code>subsets a data set to periodically specified values.</li>
</ul>

<b>Quality</b>
<ul>
  <li><code>mae()</code> calculate the mean absolute error.</li>
  <li><code>mape()</code> calculates the mean absolute percentage error.</li>
  <li><code>mse()</code> calculates the mean squared error.</li>
  <li><code>rmse()</code> calculates the root mean square error.</li>
  <li><code>huber()</code> calculates the Huber loss.</li>
  <li><code>log_cosh()</code> calculates the log-cosh loss.</li>
  <li><code>vc()</code> calculates the variance coefficient.</li>
  <li><code>accuracy()</code> calculates the fraction of correct predictions.</li>
</ul>

<b>Machine Learning (ML)</b>
<ul>
  <li><code>cross_validation_split()</code> splits an entire data set into k folds.</li>
  <li><code>naive_forecast()</code> predicts values for a data series based upon random walk without and with drifts.</li>
  <li><code>euclidean_distance()</code> calculates the euclidean distance between to numerical vectors.</li>
  <li><code>k_nearest_neighbors()</code> identifies the majority class of k nearest neighbors of a query/test instance.</li>
</ul>

<b>Single & Multi Layer Perceptron (SLP, MLP)</b>
<ul>
  <li><code>vector.as.numeric()</code> converts a vector into a vector with numeric values.</li>
  <li><code>as.ANN.matrix()</code> converts a data set into a matrix with adjusted character values and factor levels to their numeric indices if necessary.</li>
  <li><code>vector.as.ANN.matrix()</code> transforms a vector into a ANN compatible matrix.</li>
  <li><code>as.tensor</code> transforms data into a n-dimensional tensor (array).</li>
  <li><code>as.tensor.1D()</code> transforms data into a one-dimensional tensor (vector).</li>
  <li><code>as.tensor.2D()</code> transforms data into a two-dimensional tensor (matrix).</li>
  <li><code>as.tensor.3D()</code> transforms data into a three-dimensional tensor.</li>
  <li><code>as.MLP.X()</code> creates a 2-dimensional feature array with the dimensions samples and units.</li>
  <li><code>as.MLP.Y()</code> creates a 2-dimensional outcome array with the dimensions samples and units.</li>
  <li><code>get.MLP.X.units()</code> returns the number of feature units.</li>
  <li><code>get.MLP.Y.units()</code> returns the number of outcome units.</li>
  <li><code>build.MLP()</code> builds a sequential SLP/MLP model with stacked dense layers and optionally dropout layers.</li>
  <li><code>fit.MLP()</code> encapsulates building and fitting a SLP/MLP model.</li>
</ul>

<b>Reccurent Neural Network (RNN)</b>
<ul>
  <li><code>get.LSTM.XY()</code> extracts features and outcomes from a data set in a LSTM compatible preformat.</li>
  <li><code>get.period_shift()</code> calculates the period shift for a univariate and multivariate time series.</li>
  <li><code>start.invert_differencing()</code> determines the start index for invert differencing.</li>
  <li><code>as.lag()</code> transfers a lag from ARIMA(X) to a corresponding lag used for LSTM modeling.</li>
  <li><code>as.timesteps()</code> transfers a lag to a corresponding timesteps value.</li>
  <li><code>as.LSTM.X()</code> resamples a feature matrix into a 3-dimensional feature array with the dimensions samples, timesteps and units.</li>
  <li><code>as.LSTM.Y()</code> creates a 2-dimensional outcome array with the dimensions samples and units.</li>
  <li><code>get.LSTM.X.samples()</code> returns the number of feature samples.</li>
  <li><code>get.LSTM.X.timesteps()</code> returns the number of timesteps used within the resampled feature matrix.</li>
  <li><code>get.LSTM.X.units()</code> returns the number of feature units.</li>
  <li><code>get.LSTM.Y.samples()</code> returns the number of outcome samples.</li>
  <li><code>get.LSTM.Y.timesteps()</code> returns the number of outcome timesteps if outcome is a sequence.</li>
  <li><code>get.LSTM.Y.units()</code> returns the number of outcome units.</li>
  <li><code>as.LSTM.data.frame()</code> restructures a resampled feature matrix and an outcome matrix to a data.frame.</li>
  <li><code>build.LSTM()</code> builds a sequential LSTM model with stacked LSTM layers and optionally dropout layers.</li>
  <li><code>fit.LSTM()</code> encapsulates building and fitting a LSTM model.</li>
  <li><code>predict.ANN()</code> predicts with different ANN models like SLP/MLP or LSTM.</li>
  <li><code>as.LSTM.period_outcome()</code> returns a data.frame with period column and actual outcome column for quality assurance and graphical illustration purposes.</li>
</ul>

<b>Convolutional Neural Network (CNN)</b>
<ul>
  <li><code>load.image_features()</code> loads images from storage medium and creates a 4-dimensional array with the dimensions samples, height, width and channels.</li>
</ul>

<b>Self-Organizing Map (SOM)</b>
<ul>
  <li><code>fit.SOM()</code> encapsulates a complete SOM analysis.</li>
</ul>
