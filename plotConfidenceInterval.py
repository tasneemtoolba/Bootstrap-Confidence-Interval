import matplotlib.pyplot as plt
import numpy as np

# Data
# data = [
#     ['a', 4.69490776636183, 4.19859386517485, 5.32532714172712, 'Goel-Okumoto'],
#     ['b', 0.00666381699352339, 0.00394258565345935, 0.0374214657687726, 'Goel-Okumoto'],
#     ['a', 4.74915501855451, 4.31249984960778, 9.47289306196364, 'Weibull'],
#     ['b', 3.70671966654675e-05, 3.13554435226535e-09, 0.031299935504117, 'Weibull'],
#     ['c', 1.88577891446878, 0.515862523863253, 3.85335951119596, 'Weibull'],
#     ['a', 4.79814873794648, 4.20402142652626, 13.4545940502382, 'W more S-Shaped'],
#     ['b', 0.00195348549402132, 3.80542428860189e-08, 0.204257324755152, 'W more S-Shaped'],
#     ['c', 1.29548210672153, 0.304699108541422, 3.49988519330714, 'W more S-Shaped'],
#     # ['a', 342.259895257304, 87.2773178758816, 597.553859289556, 'Yamada_Raleigh'],
#     # ['b', 0.0128637894216985, 0.00712281819643572, 0.0554551832644858, 'Yamada_Raleigh'],
#     # ['c', 0.00135141678168515, 0.000338826505600534, 0.0249208598240227, 'Yamada_Raleigh']
# ]
data = [
 ['a' , 15.18 , 13.92 , 17.53,'GO'],
 ['b' , 0.02  , 0.01  , 0.03 ,'GO'],
 ['a' , 14.40 , 13.29 , 17.14,'L'],
 ['b' , 3.13  , 2.14  , 4.59 ,'L'],
 ['c' , 0.04  , 0.03  , 0.06 ,'L'],
 ['a' , 14.33 , 13.50 , 16.23,'W'],
 ['b' , 0.05  , 0.01  , 0.09 ,'W'],
 ['c' , 1.69  , 1.23  , 2.16 ,'W'],
 ['a' , 14.86 , 13.73 , 18.92,'WS'],
 ['b' , 0.45  , 0.44  , 0.49 ,'WS'],
 ['c' , 0.89  , 0.61  , 1.07 ,'WS'],
    # ['a', 342.259895257304, 87.2773178758816, 597.553859289556, 'Yamada_Raleigh'],
    # ['b', 0.0128637894216985, 0.00712281819643572, 0.0554551832644858, 'Yamada_Raleigh'],
    # ['c', 0.00135141678168515, 0.000338826505600534, 0.0249208598240227, 'Yamada_Raleigh']
]

# Separate the data by parameter variable
parameters = {}
for row in data:
    param = row[0]
    if param not in parameters:
        parameters[param] = []
    parameters[param].append(row[1:])

# Plotting
fig, ax = plt.subplots()
print(parameters)
# Iterate over each parameter
for param, values in parameters.items():
    medians = [val[0] for val in values]
    lower_bounds = [val[1] for val in values]
    upper_bounds = [val[2] for val in values]

    x = np.arange(len(medians))
    # Plot the error bars
    ax.errorbar(x, medians, yerr=[lower_bounds, upper_bounds], fmt='o', label=param)

    # ax.errorbar(x_pos, medians, yerr=[np.array(medians)-np.array(lower_bounds), np.array(upper_bounds)-np.array(medians)], fmt='none', capsize=5)

# Configure the plot
ax.set_xticks(np.arange(len(parameters['a'])))
ax.set_xticklabels([val[3] for val in parameters['a']])
ax.set_ylabel('Parameter Value')
ax.set_xlabel('Growth Model')
ax.legend()

plt.show()