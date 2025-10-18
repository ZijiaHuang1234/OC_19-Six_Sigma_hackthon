app.R: The Shiny front-end. It defines the user interface, manages file uploads, and displays prediction results interactively.                                                  
backend.R: The backend computation module. It loads the trained regression or XGBoost model, processes uploaded weather data, and returns predicted solar power outputs.             
sgv.csv: The training dataset, containing historical solar power generation data and corresponding weather variables used for model training.                                      
test.csv: The testing dataset, used for functional verification of the entire prediction workflow. It ensures that both front-end and back-end modules work correctly together.
