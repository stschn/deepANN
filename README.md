<!-- # deepANN -->
<h2>Overview deepANN</h2>
Machine and Deep Learning Toolbox

This R library is currently structured in form of the following functional families:

<b>Dummy</b>
<ul>
  <li><code>dummify()</code> creates dummy variables for non-metric variables.</li>
  <li><code>dummify_multilabel()</code> creates dummy variables for multi-label character variables.</li>
  <li><code>effectcoding()</code> changes a binary 0/1 encoded variable into a -1/1 encoded variable.</li>
  <li><code>sparse_encode()</code> builds a simple numeric encoded array.</li>
  <li><code>one_hot_encode()</code> builds a one-hot vector in form of a matrix.</li>
  <li><code>one_hot_decode()</code> restores a one-hote encoded matrix to a single vector.</li>
  <li><code>append_rows()</code> appends dummy rows.</li>
  <li><code>remove_columns()</code> removes columns with only one specific value.</li>
</ul>

<b>Encoder</b>
<ul>
  <li><code>class Encoder</code> is the base class for all encoding classes.</li>
  <li><code>class LabelEncoder</code> used for encoding labels between 0 and n_labels-1.</li>
  <li><code>class OneHotEncoder</code> used for encoding labels as a one-hot numeric array.</li>
  <li><code>class LabelBinarizer</code> used for encoding a categorical variable as a label indicator matrix.</li>
  <li><code>class MultiLabelBinarizer</code> used for encoding multi-labels as a multiple binary numeric matrix.</li>
</ul>

<b>Sampler</b>
The classes based on R6 class <code>Sampler</code> offer variant ways to balance data sets.
<ul>
  <li><code>class OverSampler</code>.</li>
  <li><code>oversampling()</code> a class wrapper function.</li>
  <li><code>class RandomOverSampler</code>.</li>
  <li><code>random_oversampling()</code> a class wrapper function.</li>
  <li><code>class SMOTE</code>.</li>
  <li><code>smote()</code> a class wrapper function.</li>
  <li><code>class UnderSampler</code>.</li>
  <li><code>undersampling()</code> a class wrapper function.</li>
  <li><code>class RandomUnderSampler</code>.</li>
  <li><code>random_undersampling()</code> a class wrapper function.</li>
</ul>

<b>Selection</b>
<ul>
  <li><code>data_split()</code> splits data into train and test subsets.</li>
  <li><code>train_test_split()</code> splits a sequence of data into random train and test subsets.</li>
</ul>

<b>Outlier</b>
<ul>
  <li><code>outlier()</code> detects and optionally replaces outliers thru the methods quartiles (from Tukey (1977)), mean (maximum likelihood estimation) or median (scaled median absolute deviation).</li>
  <li><code>outlier_dataset()</code> replaces outliers within a data set.</li>
  <li><code>winsorize()</code> winsorizes a numeric vector.</li>
</ul>

<b>Scaling</b>
<ul>
  <li><code>class Scaler</code> is the base class for all scaler classes.</li>
  <li><code>class MinMaxScaler</code> used for transforming features by scaling each feature to a given range.</li>
  <li><code>class StandardScaler</code> used for transforming features by removing the mean and scaling to unit standard deviation.</li>
  <li><code>scale_minmax()</code> scales a numeric vector thru min-max scaling.</li>
  <li><code>scale_zscore()</code> scales a numeric vector thru z-score scaling.</li>
  <li><code>scale_center()</code> scales a numeric vector thru (mean) centering.</li>
  <li><code>scale_log()</code> scales a numeric vector thru log transformation.</li>
  <li><code>scaling()</code> encapsulates the different types of scaling.</li>
  <li><code>scale_dataset()</code> scales a data set with a specific scale type.</li>
  <li><code>scale_train_test()</code> scales a train and a test data set with a specific scale type.</li>
</ul>

<b>Time Series</b>
<ul>
  <li><code>get_season()</code> delivers corresponding seasons for a given date vector.</li>
  <li><code>lags()</code> builds a lagged data set.</li>
  <li><code>stationary()</code> creates a stationary data series thru differencing.</li>
  <li><code>invert_differencing()</code> inverts a differenced data series.</li>
  <li><code>diffinv_simple()</code> inverts a simple-differenced data series.</li>
  <li><code>diff_log()</code> creates a log-differenced data series.</li>
  <li><code>diffinv_log()</code> inverts a log-differenced data series.</li>
  <li><code>diff_percentage()</code> creates a percentage-differenced data series.</li>
  <li><code>diffinv_percentage()</code> inverts a percentage-differenced data series.</li>
  <li><code>period()</code>subsets a data set/time series to periodically specified values.</li>
  <li><code>partition()</code>subsets a data set/time series into several slices.</li>
</ul>

<b>Metrics</b>
<ul>
  <li><code>stderror()</code> calculates the standard error.</li>
  <li><code>sse()</code> calculates the sum of squared errors.</li>
  <li><code>mae()</code> calculates the mean absolute error.</li>
  <li><code>mape()</code> calculates the mean absolute percentage error.</li>
  <li><code>wmape()</code> calculates the weighted mean absolute percentage error.</li>
  <li><code>wape()</code> calculates the weighted average percentage error.</li>
  <li><code>mse()</code> calculates the mean squared error.</li>
  <li><code>msle()</code> calculates the mean squared logarithmic error.</li>
  <li><code>rmse()</code> calculates the root mean square error.</li>
  <li><code>rmsle()</code> calculates the root mean square logarithmic error.</li>
  <li><code>rmspe()</code> calculates the root mean square percentage error.</li>
  <li><code>huber_loss()</code> calculates the Huber loss.</li>
  <li><code>log_cosh_loss()</code> calculates the log-cosh loss.</li>
  <li><code>quantile_loss()</code> calculates the quantile loss.</li>
  <li><code>vc()</code> calculates the variance coefficient.</li>
  <li><code>accuracy()</code> calculates the accuracy for a single-label or multi-label classification task.</li>
  <li><code>dice()</code> calculates the Dice coefficient.</li>
  <li><code>iou()</code> calculates the Intersection-over-Union (IoU) coefficient.</li>
  <li><code>gini_impurity()</code> calculates the Gini impurity.</li>
  <li><code>entropy()</code> calculates the Shannon entropy.</li>
  <li><code>cross_entropy()</code> calculates the cross-entropy.</li>
  <li><code>erf()</code> defines error function (from MATLAB).</li>
  <li><code>erfc()</code> defines complementary error function (from MATLAB).</li>
  <li><code>erfinv()</code> defines inverse error function (from MATLAB).</li>
  <li><code>erfcinv()</code> defines inverse complementary error function (from MATLAB).</li>
</ul>

<b>Utils</b>
<ul>
  <li><code>re.factor()</code> renews a factor object.</li>
  <li><code>var_pop()</code> calculates the population variance.</li>
  <li><code>sd_pop()</code> calculates the population standard deviation.</li>
  <li><code>radian()</code> converts degrees to radians.</li>
  <li><code>degree()</code> converts radians to degrees.</li>
  <li><code>distance()</code> calculates the distance between two numeric vectors.</li>
  <li><code>similarity()</code> calculates the similarity between two numeric or logical vectors.</li>
  <li><code>probability()</code> computes the probability of a categorical or continuous variable.</li>
  <li><code>vector_as_numeric()</code> converts a vector into a vector with numeric values.</li>
  <li><code>list_as_numeric()</code> recursively transforms the objects of a list into numeric values.</li>
  <li><code>as_ANN_matrix()</code> converts a data set into a matrix with adjusted character values and factor levels to their numeric indices if necessary.</li>
  <li><code>vector_as_ANN_matrix()</code> transforms a vector into a ANN compatible matrix.</li>
  <li><code>random_seed()</code> random number generator for reproducible results with Tensorflow/Keras.</li>
</ul>

<b>Machine Learning (ML)</b>
<ul>
  <li><code>cross_validation_split()</code> splits an entire data set into k folds.</li>
  <li><code>naive_forecast()</code> predicts values for a data series based upon random walk without and with drifts.</li>
  <li><code>k_nearest_neighbors()</code> identifies the categorical or continuous response and probability distributions of k nearest neighbors where appropriate of a query instance.</li>
  <li><code>moving_average()</code> calculates the (weighted) moving average.</li>
  <li><code>naive_bayes()</code> and <code>predict.naivebayes()</code> computes and predicts numeric values for classification solutions based on Bayes' theorem.</li>
  <li><code>decision_tree()</code> and <code>predict.decisiontree()</code> builds up a decision tree and predicts categorical values for classification solutions. <code>treeheight()</code> computes the height of a tree, <code>treedepth()</code> the depth of a tree.</li>
  <li><code>predict.kmeans()</code> predicts kmeans cluster for feature data.</li>
</ul>

<b>Single & Multi Layer Perceptron (SLP, MLP)</b>
<ul>
  <li><code>nsamples()</code> extracts the number of samples within a data structure, usually a tensor.</li>
  <li><code>nunits()</code> extracts the number of units within a data structure, usually a tensor.</li>
  <li><code>ntimesteps()</code> extracts the number of timesteps within a data structure, usually a tensor.</li>
  <li><code>nsubsequences()</code> extracts the number of subsequences within a data structure, usually a tensor.</li>
  <li><code>as_tensor_1d()</code> transforms data into a one-dimensional tensor (vector).</li>
  <li><code>as_tensor_2d()</code> transforms data into a two-dimensional tensor (matrix).</li>
  <li><code>as_tensor_3d()</code> transforms data into a three-dimensional tensor.</li>
  <li><code>as_MLP_X()</code> creates a 2D feature array with the dimensions samples and units.</li>
  <li><code>as_MLP_Y()</code> creates a 2D outcome array with the dimensions samples and units for a metric outcome or a one-hot vector for a non-metric outcome.</li>
  <li><code>build_MLP()</code> builds a sequential SLP/MLP model with stacked dense layers and optionally dropout layers.</li>
  <li><code>fit_MLP()</code> encapsulates fitting a SLP/MLP model.</li>
  <li><code>save_weights_ANN()</code> saves the weights of a ANN into a HDF5-file.</li>
  <li><code>load_weights_ANN()</code> loads the weights of a ANN from a HDF5-file.</li>
</ul>

<b>Reccurent Neural Network (RNN)</b>
<ul>
  <li><code>get_LSTM_XY()</code> extracts features and outcomes from a data set in a LSTM compatible preformat.</li>
  <li><code>get_period_shift()</code> calculates the period shift for a univariate and multivariate time series.</li>
  <li><code>start_invert_differencing()</code> determines the start index for invert differencing.</li>
  <li><code>as_lag()</code> transfers a lag from ARIMA(X) to a corresponding lag used for LSTM modeling.</li>
  <li><code>as_timesteps()</code> transfers a lag to a corresponding timesteps value.</li>
  <li><code>as_LSTM_X()</code> resamples a feature matrix into a 3D feature array with the dimensions samples, timesteps and units.</li>
  <li><code>as_LSTM_Y()</code> creates either a 2D array with dimensions samples and units respectively a 3D array with dimensions samples, timesteps and units for a metric outcome or a one-hot vector for a non-metric outcome.</li>
  <li><code>as_LSTM_data_frame()</code> restructures a resampled feature matrix and an outcome matrix to a data.frame.</li>
  <li><code>build_LSTM()</code> builds a sequential LSTM model with stacked LSTM layers and optionally dropout layers.</li>
  <li><code>fit_LSTM()</code> encapsulates fitting a LSTM model.</li>
  <li><code>predict_ANN()</code> predicts with different ANN models like SLP/MLP or LSTM.</li>
  <li><code>as_LSTM_period_outcome()</code> returns a data.frame with period column and actual outcome column for quality assurance and graphical illustration purposes.</li>
</ul>

<b>Convolutional Neural Network (CNN)</b>
<ul>
  <li><code>images_load()</code> load images from different sources.</li>
  <li><code>images_resize()</code> resizes loaded images.</li>
  <li><code>as_images_array()</code> converts image representation into 3D array.</li>
  <li><code>as_images_tensor()</code> builds an image tensor of corresponding shape depending on the type of images (2D or 3D images).</li>
  <li><code>as_CNN_image_X()</code> creates a 4D image feature array with the dimensions samples, height, width and channels either from already given image data or from images on a storage medium.</li>
  <li><code>as_CNN_image_Y()</code> creates a one-hot vector for the image labels.</li>
  <li><code>as_CNN_temp_X()</code> resamples a feature matrix into a 3D feature array with the dimensions samples, timesteps and units or into a 4D array with the dimensions samples, subsequences, timesteps and features.</li>
  <li><code>as_CNN_temp_Y()</code> creates either a 2D outcome array with the dimensions samples and units respectively a 3D array with dimensions samples, timesteps and units for a metric outcome or a one-hot vector for a non-metric outcome.</li>
  <li><code>lenet5()</code> build a CNN model from type LeNet-5.</li>
  <li><code>alexnet()</code> build a CNN model from type AlexNet.</li>
  <li><code>zfnet()</code> build a CNN model from type ZFNet.</li>
  <li><code>vgg16()</code> build a CNN model from type VGG-16.</li>
  <li><code>vgg19()</code> build a CNN model from type VGG-19.</li>
  <li><code>resnet50()</code> build a CNN model from type ResNet-50.</li>
  <li><code>inception_v3()</code> build a CNN model from type Inception v3.</li>
  <li><code>inception_resnet_v2()</code> build a CNN model from type Inception-ResNet v2.</li>
  <li><code>mobilenet()</code> build a CNN model from type MobileNet.</li>
  <li><code>mobilenet_v2()</code> build a CNN model from type MobileNetV2.</li>
  <li><code>mobilenet_v3()</code> build a CNN model from type MobileNetV3.</li>
  <li><code>xception()</code> build a CNN model from type Xception.</li>
  <li><code>nasnet()</code> build a CNN model from type NASNet-A.</li>
  <li><code>unet()</code> build a CNN model from type U-Net.</li>
  <li><code>unet3d()</code> build a CNN model from type 3D U-Net.</li>
</ul>
