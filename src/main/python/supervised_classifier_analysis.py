import sys
import pandas as pd
from sklearn.metrics import classification_report
from sklearn.svm import SVC
from sklearn.feature_selection import RFE
from sklearn.model_selection import StratifiedKFold, GridSearchCV
from sklearn.pipeline import Pipeline

fold_num = 5

if __name__ == "__main__":
    file_path = sys.argv[1]
    training_path = sys.argv[2]
    testing_path = sys.argv[3]

    f_all = pd.read_csv(file_path, sep="\t")
    f_train = pd.read_csv(training_path, sep="\t")
    f_test = pd.read_csv(testing_path, sep="\t")

    features = f_all.columns
    features = features.drop("label")

    labels = f_all.label

    label_function = {l: i for i, l in enumerate(labels.unique())}

    y = labels.map(label_function).values
    X = f_all[features].values

    X_train = f_train[features].values
    y_train = f_train.label.map(label_function).values

    X_test = f_test[features].values
    y_test = f_test.label.map(label_function).values

    cv_folds = StratifiedKFold(5)

    rfe = RFE(estimator=SVC(kernel="linear"), step=1)

    pipeline = Pipeline([("feature_selection", rfe), ('classification', SVC(class_weight="balanced"))])

    param_grid = {
        "feature_selection__n_features_to_select":[None, 7, 5, 3],
        'classification__kernel':('linear', 'rbf', 'sigmoid'),
        'classification__C':[0.01, 0.1, 0.5, 1, 5, 10]
    }

    grid_search = GridSearchCV(pipeline, param_grid, cv = cv_folds, scoring='f1_micro', n_jobs=4)

    grid_search.fit(X, y)

    print "Best features of the classifier"
    print grid_search.best_params_

    print "Features selected out of %i" % (X.shape[1]-1)
    print features[grid_search.best_estimator_.named_steps.feature_selection.support_]

    selected_estimator = grid_search.best_estimator_

    # Fit to the training data
    selected_estimator.fit(X_train, y_train)
    y_pred = selected_estimator.predict(X_test)

    print classification_report(y_test, y_pred, target_names=labels.unique())



    #print features[rfecv.support_]