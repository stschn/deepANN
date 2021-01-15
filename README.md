<!-- # deepANN -->
<h2>Overview deepANN</h2>
Neural Network Toolbox

This R library is currently structured in form of the following functional families:

<b>Dummy</b>
<ul>
  <li><code>dummify()</code> creates dummy variables for non-metric variables.</li>
  <li><code>dummify.multilabel()</code> creates dummy variables for multi-label character variables.</li>
  <li><code>effectcoding()</code> changes a binary 0/1 encoded variable into a -1/1 encoded variable.</li>
  <li><code>one_hot_encode()</code> builds a one-hot vector in form of a matrix.</li>
  <li><code>one_hot_decode()</code> restores a one-hote encoded matrix to a single vector.</li>
  <li><code>resample.imbalanced()</code> creates dummy rows for an imbalanced data set thru the techniques oversampling, undersampling or SMOTE.</li>
  <li><code>append_rows()</code> appends dummy rows.</li>
  <li><code>remove_columns()</code> removes columns with only one specific value.</li>
</ul>

<b>Outlier</b>
<ul>
  <li><code>outlier()</code> detects and optionally replaces outliers thru the methods quartiles (from Tukey (1977)), mean (maximum likelihood estimation) or median (scaled median absolute deviation).</li>
  <li><code>outlier.dataset()</code> replaces outliers within a data set.</li>
  <li><code>winsorize()</code> winsorizes a numeric vector.</li>
</ul>

<b>Scaling</b>
<ul>
  <li><code>scale.minmax()</code> scales a numeric vector thru min-max scaling.</li>
  <li><code>scale.zscore()</code> scales a numeric vector thru z-score scaling.</li>
  <li><code>scale.center()</code> scales a numeric vector thru (mean) centering.</li>
  <li><code>scale.log()</code> scales a numeric vector thru log transformation.</li>
  <li><code>scaling()</code> encapsulates the different types of scaling.</li>
  <li><code>scale.dataset()</code> scales a data set with a specific scale type.</li>
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
  <li><code>mae()</code> calculates the mean absolute error.</li>
  <li><code>mape()</code> calculates the mean absolute percentage error.</li>
  <li><code>mse()</code> calculates the mean squared error.</li>
  <li><code>msle()</code> calculates the mean squared logarithmic error.</li>
  <li><code>rmse()</code> calculates the root mean square error.</li>
  <li><code>rmsle()</code> calculates the root mean square logarithmic error.</li>
  <li><code>huber_loss()</code> calculates the Huber loss.</li>
  <li><code>log_cosh_loss()</code> calculates the log-cosh loss.</li>
  <li><code>quantile_loss()</code> calculates the quantile loss.</li>
  <li><code>vc()</code> calculates the variance coefficient.</li>
  <li><code>accuracy()</code> calculates the accuracy for a single-label or multi-label classification task.</li>
  <li><code>gini_impurity()</code> calculates the Gini impurity.</li>
  <li><code>entropy()</code> calculates the Shannon entropy.</li>
  <li><code>cross_entropy()</code> calculates the cross-entropy.</li>
</ul>

<b>Machine Learning (ML)</b>
<ul>
  <li><code>var.pop()</code> calculates the population variance.</li>
  <li><code>sd.pop()</code> calculates the population standard deviation.</li>
  <li><code>cross_validation_split()</code> splits an entire data set into k folds.</li>
  <li><code>naive_forecast()</code> predicts values for a data series based upon random walk without and with drifts.</li>
  <li><code>as.radian()</code> converts degrees to radians.</li>
  <li><code>as.degree()</code> converts radians to degrees.</li>
  <li><code>distance()</code> calculates the distance between two numeric vectors.</li>
  <li><code>similarity()</code> calculates the similarity between two numeric or logical vectors.</li>
  <li><code>k_nearest_neighbors()</code> identifies the categorical or continuous response and probability distributions of k nearest neighbors where appropriate of a test instance.</li>
  <li><code>moving_average()</code> calculates the (weighted) moving average.</li>
  <li><code>predict.kmeans()</code> predicts kmeans cluster for feature data.</li>
  <li><code>erf()</code> defines error function (from MATLAB).</li>
  <li><code>erfc()</code> defines complementary error function (from MATLAB).</li>
  <li><code>erfinv()</code> defines inverse error function (from MATLAB).</li>
  <li><code>erfcinv()</code> defines inverse complementary error function (from MATLAB).</li>
</ul>

<b>Single & Multi Layer Perceptron (SLP, MLP)</b>
<ul>
  <li><code>vector.as.numeric()</code> converts a vector into a vector with numeric values.</li>
  <li><code>as.ANN.matrix()</code> converts a data set into a matrix with adjusted character values and factor levels to their numeric indices if necessary.</li>
  <li><code>vector.as.ANN.matrix()</code> transforms a vector into a ANN compatible matrix.</li>
  <li><code>flatten</code> flattens data into a one-dimensional array.</li>
  <li><code>as.numpy</code> transforms a vector into a n-dimensional numpy array.</li>
  <li><code>as.tensor</code> transforms data into a n-dimensional tensor (array).</li>
  <li><code>as.tensor.1D()</code> transforms data into a one-dimensional tensor (vector).</li>
  <li><code>as.tensor.2D()</code> transforms data into a two-dimensional tensor (matrix).</li>
  <li><code>as.tensor.3D()</code> transforms data into a three-dimensional tensor.</li>
  <li><code>as.MLP.X()</code> creates a 2D feature array with the dimensions samples and units.</li>
  <li><code>as.MLP.Y()</code> creates a 2D outcome array with the dimensions samples and units for a metric outcome or a one-hot vector for a non-metric outcome.</li>
  <li><code>get.MLP.X.samples()</code> returns the number of feature samples.</li>
  <li><code>get.MLP.X.units()</code> returns the number of feature units.</li>
  <li><code>get.MLP.Y.samples()</code> returns the number of outcome samples.</li>
  <li><code>get.MLP.Y.units()</code> returns the number of outcome units.</li>
  <li><code>build.MLP()</code> builds a sequential SLP/MLP model with stacked dense layers and optionally dropout layers.</li>
  <li><code>fit.MLP()</code> encapsulates fitting a SLP/MLP model.</li>
</ul>

<b>Reccurent Neural Network (RNN)</b>
<ul>
  <li><code>get.LSTM.XY()</code> extracts features and outcomes from a data set in a LSTM compatible preformat.</li>
  <li><code>get.period_shift()</code> calculates the period shift for a univariate and multivariate time series.</li>
  <li><code>start.invert_differencing()</code> determines the start index for invert differencing.</li>
  <li><code>as.lag()</code> transfers a lag from ARIMA(X) to a corresponding lag used for LSTM modeling.</li>
  <li><code>as.timesteps()</code> transfers a lag to a corresponding timesteps value.</li>
  <li><code>as.LSTM.X()</code> resamples a feature matrix into a 3D feature array with the dimensions samples, timesteps and units.</li>
  <li><code>get.LSTM.X.samples()</code> returns the number of feature samples.</li>
  <li><code>get.LSTM.X.timesteps()</code> returns the number of timesteps used within the resampled feature matrix.</li>
  <li><code>get.LSTM.X.units()</code> returns the number of feature units.</li>
  <li><code>as.LSTM.Y()</code> creates either a 2D array with dimensions samples and units respectively a 3D array with dimensions samples, timesteps and units for a metric outcome or a one-hot vector for a non-metric outcome.</li>
  <li><code>get.LSTM.Y.samples()</code> returns the number of outcome samples.</li>
  <li><code>get.LSTM.Y.timesteps()</code> returns the number of outcome timesteps if outcome is a sequence.</li>
  <li><code>get.LSTM.Y.units()</code> returns the number of outcome units.</li>
  <li><code>as.LSTM.data.frame()</code> restructures a resampled feature matrix and an outcome matrix to a data.frame.</li>
  <li><code>build.LSTM()</code> builds a sequential LSTM model with stacked LSTM layers and optionally dropout layers.</li>
  <li><code>fit.LSTM()</code> encapsulates fitting a LSTM model.</li>
  <li><code>predict.ANN()</code> predicts with different ANN models like SLP/MLP or LSTM.</li>
  <li><code>as.LSTM.period_outcome()</code> returns a data.frame with period column and actual outcome column for quality assurance and graphical illustration purposes.</li>
  <li><code>save_weights.ANN()</code> saves the weights of a ANN into a HDF5-file.</li>
  <li><code>load_weights.ANN()</code> loads the weights of a ANN from a HDF5-file.</li>
</ul>

<b>Convolutional Neural Network (CNN)</b>
<ul>
  <li><code>as.CNN.image.X()</code> creates a 4D image feature array with the dimensions samples, height, width and channels either from already given image data or from images on a storage medium.</li>
  <li><code>as.CNN.image.Y()</code> creates a one-hot vector for the image labels.</li>
  <li><code>get.CNN.image.X.samples()</code> returns the number of images (feature samples).</li>
  <li><code>get.CNN.image.X.height()</code> returns the height of the images.</li>
  <li><code>get.CNN.image.X.width()</code> returns the width of the images.</li>
  <li><code>get.CNN.image.X.channels()</code> returns the number of color channels of the images.</li>
  <li><code>get.CNN.image.Y.samples()</code> returns the number of image samples or labels.</li>
  <li><code>get.CNN.image.Y.units()</code> returns the number of output units.</li>
  <li><code>as.CNN.temp.X()</code> resamples a feature matrix into a 3D feature array with the dimensions samples, timesteps and units or into a 4D array with the dimensions samples, subsequences, timesteps and features.</li>
  <li><code>get.CNN.temp.X.samples()</code> returns the number of feature samples.</li>
  <li><code>get.CNN.temp.X.timesteps()</code> returns the number of timesteps used within the resampled feature matrix.</li>
  <li><code>get.CNN.temp.X.units()</code> returns the number of feature units.</li>
  <li><code>as.CNN.temp.Y()</code> creates either a 2D outcome array with the dimensions samples and units respectively a 3D array with dimensions samples, timesteps and units for a metric outcome or a one-hot vector for a non-metric outcome.</li>
 <li><code>get.CNN.temp.Y.samples()</code> returns the number of outcome samples.</li>
  <li><code>get.CNN.temp.Y.timesteps()</code> returns the number of outcome timesteps if outcome is a sequence.</li>
  <li><code>get.CNN.temp.Y.units()</code> returns the number of outcome units.</li>
</ul>

<b>Self-Organizing Map (SOM)</b>
<ul>
  <li><code>fit.SOM()</code> encapsulates a complete SOM analysis.</li>
</ul>
