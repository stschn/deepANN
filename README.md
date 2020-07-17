<!-- # deepANN -->
<h2>Overview deepANN</h2>
Neural Network Toolbox

This R library is currently structured in form of the following functional families:

<b>Dummy</b>
<ul>
  <li><code>dummify()</code></li>
  <li><code>effectcoding()</code></li>
  <li><code>one_hot_encode()</code></li>
  <li><code>one_hot_decode()</code></li>
  <li><code>resample.imbalanced()</code></li>
</ul>

<b>Outlier</b>
<ul>
  <li><code>outlier()</code></li>
  <li><code>winsorize()</code></li>
</ul>

<b>Scaling</b>
<ul>
  <li><code>normalize()</code></li>
  <li><code>denormalize()</code></li>
  <li><code>normalize_data()</code></li>
</ul>

<b>TimeSeries</b>
<ul>
  <li><code>get.season()</code></li>
  <li><code>lags()</code></li>
  <li><code>build.stationary()</code></li>
  <li><code>invert_differencing()</code></li>
  <li><code>period()</code></li>
</ul>

<b>Quality</b>
<ul>
  <li><code>mae()</code></li>
  <li><code>mape()</code></li>
  <li><code>mse()</code></li>
  <li><code>rmse()</code></li>
  <li><code>vc()</code></li>
</ul>

<b>Machine Learning (ML)</b>
<ul>
  <li><code>cross_validation_split()</code></li>
  <li><code>naive_forecast()</code></li>
  <li><code>euclidean_distance()</code></li>
  <li><code>k_nearest_neighbors()</code></li>
</list>

<b>Single & Multi Layer Perceptron (SLP, MLP)</b>
<ul>
  <li><code>as.ANN.matrix()</code></li>
  <li><code>as.MLP.X()</code></li>
  <li><code>as.MLP.Y()</code></li>
  <li><code>get.MLP.X.units()</code></li>
  <li><code>get.MLP.Y.units()</code></li>
  <li><code>build.MLP()</code></li>
  <li><code>fit.MLP()</code></li>
  <li><code>predict.MLP()</code></li>
</ul>

<b>Reccurent Neural Network (RNN), Long Short-Term Memory (LSTM)</b>
<ul>
  <li><code>get.LSTM.XY()</code></li>
  <li><code>get.LSTM.period_shift()</code></li>
  <li><code>start.LSTM.invert_differencing()</code></li>
  <li><code>as.lag()</code></li>
  <li><code>as.timesteps()</code></li>
  <li><code>as.LSTM.X()</code></li>
  <li><code>as.LSTM.Y()</code></li>
  <li><code>get.LSTM.X.samples()</code></li>
  <li><code>get.LSTM.X.timesteps()</code></li>
  <li><code>get.LSTM.X.units()</code></li>
  <li><code>get.LSTM.Y.samples()</code></li>
  <li><code>get.LSTM.Y.units()</code></li>
  <li><code>as.LSTM.data.frame()</code></li>
  <li><code>build.LSTM()</code></li>
  <li><code>fit.LSTM()</code></li>
  <li><code>predict.LSTM()</code></li>
  <li><code>as.LSTM.period_outcome()</code></li>
</ul>

<b>Convolutional Neural Network (CNN)</b>
<ul>
  <li><code>load.image_features()</code></li>
</ul>

<b>Self-Organizing Map (SOM)</b>
<ul>
  <li><code>fit.SOM()</code></li>
</ul>
