# Aim: To determine the sample set for which the sample confidence interval is within an expanded population confidence interval
# for different arrangements of a sequential dataset

from sklearn import metrics # for ROC and AUC calculations
import pandas as pd # to work with csv files
from scipy.stats import sem, t # for confidence intervals
from scipy import mean
import scipy.stats
from scipy import stats
from scipy.stats import norm
import numpy as np
import AUC_CI_DeLong
confidence = 0.95

# Create dataframe to store AUCs (Area Under R O Curve)
dfAUC = pd.DataFrame(columns = ['AUC'])


# # Make dataframe from csv file ---------------------------------------------------------------------------------------
#df = pd.read_csv("C:/Users/zhizh/OneDrive - Stop TB Partnership/UNOPS/10 Paper Writing/CAR software/02 Bangladesh/CAR-Bangladesh/ProjectMachineBGD/Nepal_Cameroon.csv", usecols = ['ResearchID.x', 'XPERT_pos', 'CAD4TB6', 'qXR2', 'Lunit', 'Result.Date'])
df = pd.read_csv("C:/Users/tasneemn/OneDrive - Stop TB Partnership/Documents/GitHub/ProjectMachine/Nepal_Cameroon.csv", usecols = ['ResearchID.x', 'XPERT_pos', 'CAD4TB6', 'qXR2', 'Lunit', 'Result.Date'])
# Sort dataframe by result date and store
df = df.sort_values(by = 'Result.Date')
df.to_csv(r'C:/Users/tasneemn/OneDrive - Stop TB Partnership/Documents/GitHub/ProjectMachine/Nepal_Cameroon_sorted.csv')
print(df)
# ----------------------------------------------------------------------------------------------------------------------

# Extract population AUC & CI: CAD4TB6
# Extract true results and AI software scores from csv file and store in dataframe
Data = pd.read_csv("Nepal_Cameroon_sorted.csv", usecols = ['XPERT_pos', 'CAD4TB6'])
print(Data)

data = Data
count = 0

# Calculate population confidence interval--------------------------------------------------------------------------
alpha = .95
y_pred = data['CAD4TB6']  # AI Scores
y_true = data['XPERT_pos']  # True results

# Calculate AUC
auc, auc_cov = AUC_CI_DeLong.delong_roc_variance(y_true, y_pred)

# Calculate confidence intervals
auc_std = np.sqrt(auc_cov)
lower_upper_q = np.abs(np.array([0, 1]) - (1 - alpha) / 2)

ci = scipy.stats.norm.ppf(
    lower_upper_q,
    loc=auc,
    scale=auc_std)

ci[ci > 1] = 1

print('AUC:', auc)
print("population confidence interval: \n lower limit: ", ci[0], " \n upper limit: ", ci[1])
# --------------------------------------------------------------------------------------------------------------------

# For each arrangement of the dataframe
for i in range(0, (len(data)-1)):
    print('Dataset Arrangement No: (i)', i)
    j = 50 # First set of {true result-AI score} needs to be greater than 0 for ROC calculations # even

    # for each {true result-AI score} pair in dataframe
    for j in range(j, (len(data)+1)):
        # Take section of {true result-AI score} pairs - increase by one pair each iteration
        y_pred_S = data.iloc[0:j, [1]] # AI Score
        y_pred_S = y_pred_S.iloc[:, 0] # Convert dataframe to series
        y_true_S = data.iloc[0:j, [0]] # True results
        y_true_S = y_true_S.iloc[:, 0]

        # Check if there is one in [0:j] set in y_true_S (true results)
        verify = y_true_S.isin([1]).any()
        if verify == True: # If true result sample contains both 0s and 1s
            # Calculate AUC
            aucS, auc_covS = AUC_CI_DeLong.delong_roc_variance(y_true_S, y_pred_S)

            auc_stdS = np.sqrt(auc_covS)
            lower_upper_q_S = np.abs(np.array([0, 1]) - (1 - alpha) / 2)

            # Calculate confidence interval
            ci_S = scipy.stats.norm.ppf(lower_upper_q_S, loc=aucS, scale=auc_stdS)

            ci_S[ci_S > 1] = 1

            # Record AUC
            dfAUC = dfAUC.append({'AUC':aucS, 'size': j}, ignore_index=True)

            #dfAUC = dfAUC.append({}, ignore_index=True)

<<<<<<< HEAD
        # Record AUC
        dfAUC = dfAUC.append({'AUC':aucS, 'size': j}, ignore_index=True)
<<<<<<< Updated upstream
=======
        if (ci_S[0] > (ci[0] - 0.05)) & (ci_S[1] < (ci[1] + 0.05)):
            print('AUC:', aucS)
            print("sample confidence interval: \n lower limit: ", ci_S[0], " \n upper limit: ", ci_S[1])

>>>>>>> Stashed changes
=======
            # If sample CI lower limit is greater than population CI lower limit and
            # if sample CI upper limit is lower than population CI upper limit
            if (ci_S[0] > (ci[0] - 0.05)) & (ci_S[1] < (ci[1] + 0.05)):
                print('Sample set (j)', j)
                print('lowerlimit', (ci[0] - 0.05), 'upperlimit', (ci[1] + 0.05))
                print('AUC:', aucS)
                print("sample confidence interval: \n lower limit: ", ci_S[0], " \n upper limit: ", ci_S[1])
                break # Stop when condition is met, move on to next sample set
>>>>>>> 4d78f4c2e40925fdcc89a9875cedd5044341d359


    # Move the first row of the data frame to the end to create a new dataset
    dfAUC = dfAUC.append({'dataset arrangement': i}, ignore_index=True)
    count = count + 1
    i += 1
    first = data.iloc[1:len(data)] # Create dataframe containing all rows except the first
    last = data.iloc[0] # Extract first row of dataframe
    data = first.append(last) # Append first row to end
    #print(data)

print(dfAUC)



