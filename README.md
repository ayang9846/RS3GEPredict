# Summary
The Grand Exchange is an in-player marketplace for Runescape 3 and Old School Runescape players to sell and purchase in-game items. 
Not only is it often used to sell and purchase valuable gear, but it also serves as a speculative platform for merchanting/flipping (for a more formal explaination, see [the Runescape wiki's page](https://runescape.wiki/w/Trading_and_merchanting_guide)).

This repo represents 2022 code used to conduct three primary goals:
1) Retriving time series Grand Exchange prices (the in-game marketplace for Runescape 3 items). (GEPredict- GE Price Extractor)
2) Modeling and predicting the price of one item using an ARIMA approach. (GEPredict- Time Series Analysis)
3) Modeling and predicting the price of one item using a neural net. (GEPredict- Machine Learning Model v2)

It covers two modeling implementations: one in R, and one in Python.

# Examples
<p>
  <img width="512" src="https://imgur.com/Z9zOJk1.png">
  
  <em>Python Scikit-learn implementation of neural net.</em>
</p>

<p>
  <img width="512" src="https://imgur.com/4bWvQ0i.png">
  
  <em>Example Holt-Winters filtering on price time series.</em>
</p>

<p>
  <img width="512" src="https://imgur.com/nKvAwP4.png">
  
  <em>Example ARIMA projection on price time series.</em>
</p>

<p>
  <img width="512" src="https://imgur.com/QCw7slW.png">
  
  <em>R implementation of an autoregressive neural net for time series modeling.</em>
</p>
