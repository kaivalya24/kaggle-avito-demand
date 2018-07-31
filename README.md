Introduction:
Avito is an online classified advertisement company based in Russia.
The Avito Demand Prediction challenge required the competitior to use Image Processing and Text Mining to predict the demand of an online advertisement based mainly on the ad image, ad title and description. Other atributes like the location of the ad, the date it was published and the price of the product advertised.

Tools:
I have mainly used R for all my modeling. I haven't done lot of text mining as most of the data was in Russian. I extracted basic image features (image size, average color etc.,) in Python using the cv2 package. These features were extracted for a million images.

Feature engineering:
A few new columns called has_image (to indicate whether an ad has an image or not) and count of characters in the ad description and title were added to the dataset along with the the extracted image features.

Modeling:
Initially, an XGBoost model was built. Then using the caret and h2o packages, a stack of models utilizing deep learning, random forest, glm, and gbm as the base models was created. 

Conclusions:
There were many incompatibilities with the caret and h2o packages in R 3.5.0. This code can give youba very basic idea of modeling and ensembling in R and image processing in Python. If you want to win a competition or rank well, I suggest you look into Python as most competition winners use it and I was told by them, that Pyhton packages are much more advanced in function than their R counterparts.


