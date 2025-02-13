# **Credit Loan Risk Analysis**

## 📌 **Project Overview**
This project explores **financial loan data** to uncover patterns in **interest rates, loan amounts, borrower risk, and predictive modeling**. Using **Generalized Linear Models (GLM), LASSO Regression, and Chi-Square Tests**, we analyze factors influencing loan approvals, interest rates, and defaults.

## 📂 **Dataset Information**
- **Dataset:** Financial Loan Dataset from Kaggle
- **Total Observations:** 10,000 records
- **Selected Features:** 26 variables out of 55 total
- **Objective:** Predict loan default probability and analyze borrower attributes

### **📊 Key Variables**
| Variable                     | Description |
|------------------------------|------------|
| `interest_rate`              | Loan interest rate assigned to a borrower |
| `annual_income`              | Borrower’s reported annual income |
| `debt_to_income`             | Debt-to-income ratio affecting loan risk |
| `homeownership`              | Borrower’s homeownership status (Own, Rent, Mortgage) |
| `loan_status`                | Loan repayment status (Default/No Default) |
| `loan_purpose`               | Purpose of the loan (Debt Consolidation, Home Improvement, etc.) |
| `total_credit_utilized`      | Total credit used by the borrower |
| `num_mort_accounts`         | Number of mortgage accounts owned by the borrower |

---

## 📈 **Exploratory Data Analysis**
### **1️⃣ Loan Interest Rate Distributions**
- **What are the trends in loan interest rates?**
- **Are higher incomes correlated with lower interest rates?**
- **How does homeownership status affect interest rates?**

#### **📊 Interest Rate vs. Debt-to-Income Ratio**
*Higher debt-to-income ratios show increased risk, leading to higher interest rates.*

![Interest Rate vs. DTI](images/InterestRate_DTI.png)

---

## **📑 Hypothesis Testing**
### **🏠 Homeownership vs. Loan Purpose**
- **Are certain loan purposes more common among homeowners vs. renters?**
- **Do homeownership types influence the loan amounts received?**
- **Statistical Method Used:** **Chi-Square Test & ANOVA**

#### **📊 Loan Purpose by Homeownership**
*Debt consolidation is the most common loan purpose across all homeownership statuses.*

![Loan Purpose by Homeownership](images/LoanPurpose_Homeownership.png)

---

## **📉 Regression & Predictive Modeling**
### **🔮 Can we predict loan defaults using borrower attributes?**
- **Model Used:** Logistic Regression with LASSO Regularization
- **Predictor Variables:** Annual Income, Debt-to-Income Ratio, Homeownership, Employment Length

#### **📊 ROC Curve - Predicting Loan Default**
*The ROC curve shows a good model fit with an AUC of 0.72.*

![ROC Curve](images/ROC_Curve.png)

---

## **📁 Project Structure**
