import os
import pandas as pd
import numpy as np
from sklearn.pipeline import Pipeline
from sklearn.feature_selection import VarianceThreshold
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import ExtraTreesRegressor, RandomForestClassifier
from xgboost import XGBClassifier
from sklearn.impute import SimpleImputer
from sklearn.decomposition import PCA
from sklearn.metrics import accuracy_score, log_loss
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC, LinearSVC, NuSVC
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, AdaBoostClassifier, GradientBoostingClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.discriminant_analysis import QuadraticDiscriminantAnalysis
from xgboost import XGBClassifier
from sklearn.metrics import *
from sklearn.tree import DecisionTreeClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import GaussianNB, MultinomialNB, BernoulliNB
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV, ParameterGrid
from sklearn import preprocessing, svm, metrics, tree, decomposition
from sklearn.model_selection import cross_validate
from sklearn.ensemble import RandomForestClassifier, ExtraTreesClassifier, GradientBoostingClassifier, AdaBoostClassifier
from sklearn.linear_model import LogisticRegression, Perceptron, SGDClassifier
from time import time
from sklearn.model_selection import StratifiedKFold
import pickle


# Directorios donde se ubican los datos, asi como donde se guardaran los modelos y sus resultados
temp = os.path.expanduser('../../data')
results = os.path.expanduser('/results')
models = os.path.expanduser('/results/models')


# Lectura de los datos de entrenamiento y prueba
addr= os.path.join(temp, "walmart_test_trip_dummies.csv")
df_test = pd.read_csv(addr)

addr= os.path.join(temp, "walmart_train_trip_dummies.csv")
df_train = pd.read_csv(addr)

# Funcion auxiliar para crear el archivo que se subira a Kaggle
def create_submit(y_pred, nom):
    test_predictions = pd.DataFrame(y_pred)
    test_indexes = df_test.visitnumber
    test_predictions.insert(0, 'VisitNumber', test_indexes)
    test_predictions.columns = ['VisitNumber', 'TripType_3','TripType_4','TripType_5','TripType_6','TripType_7',\
    'TripType_8','TripType_9','TripType_12','TripType_14','TripType_15','TripType_18',\
    'TripType_19','TripType_20','TripType_21','TripType_22','TripType_23','TripType_24',\
    'TripType_25','TripType_26','TripType_27','TripType_28','TripType_29','TripType_30',\
    'TripType_31','TripType_32','TripType_33','TripType_34','TripType_35','TripType_36',\
    'TripType_37','TripType_38','TripType_39','TripType_40','TripType_41','TripType_42',\
    'TripType_43','TripType_44','TripType_999']

    name = "/submissions/" + str(nom) + ".csv"
    addr= os.path.join(results, name)
    test_predictions.to_csv(addr, index=False)


# Gridsearch
def define_hyper_params():
    clfs = {
        'RF': RandomForestClassifier(n_estimators=10),
        'ET': ExtraTreesClassifier(n_estimators=10, criterion='entropy'),
        'AB': AdaBoostClassifier(DecisionTreeClassifier(max_depth=1), algorithm="SAMME", n_estimators=200),
        'LR': LogisticRegression(penalty='l2', C=1e5, solver='saga',max_iter=300),
        'SVM': svm.SVC(kernel='linear', probability=True, random_state=0),
        'GB': GradientBoostingClassifier(learning_rate=0.05, subsample=0.5, max_depth=6, n_estimators=10),
        'NB': GaussianNB(),
        'DT': DecisionTreeClassifier(),
        'SGD': SGDClassifier(loss="hinge", penalty="l2"),
        'KNN': KNeighborsClassifier(n_neighbors=1),
        'XGB': XGBClassifier(objective= 'multi:softprob', num_class=38,
                eval_metric= 'mlogloss', max_delta_step= 5)
            }

    grid = {
        'RF':{'Classifier__n_estimators': [200,1000,2000], 'Classifier__max_depth': [20,50,100],
              'Classifier__max_features': ['sqrt','log2'],'Classifier__min_samples_split': [2,5,10]},
        #'LR': { 'Classifier__penalty': ['l2','elasticnet'], 'Classifier__C': [20,50,100], 'Classifier__l1_ratio': [0.3, 0.7, 0.8,0.9]},
        'LR': { 'Classifier__penalty': ['l2'], 'Classifier__C': [20], 'Classifier__l1_ratio': [0.3]},
        'SGD': { 'Classifier__loss': ['hinge','log','perceptron'], 'Classifier__penalty': ['l2','l1','elasticnet']},
        'ET': { 'Classifier__n_estimators': [1,10,100,1000,10000], 'Classifier__criterion' : ['gini', 'entropy'] ,
               'Classifier__max_depth': [1,5,10,20,50,100], 'Classifier__max_features': ['sqrt','log2'],
               'Classifier__min_samples_split': [2,5,10]},
        'AB': { 'Classifier__algorithm': ['SAMME', 'SAMME.R'], 'Classifier__n_estimators': [1,10,100,1000,10000]},
        'GB': {'Classifier__n_estimators': [1,10,100,1000,10000], 'Classifier__learning_rate' : [0.001,0.01,0.05,0.1,0.5],
               'Classifier__subsample' : [0.1,0.5,1.0], 'Classifier__max_depth': [1,3,5,10,20,50,100]},
        'NB' : {},
        'DT': {'Classifier__criterion': ['gini', 'entropy'], 'Classifier__max_depth': [1,5,10,20,50,100],
               'Classifier__max_features': ['sqrt','log2'],'Classifier__min_samples_split': [2,5,10]},
        'SVM' :{'Classifier__C' :[0.00001,0.0001,0.001,0.01,0.1,1,10],'Classifier__kernel':['linear', 'rbf']},
        'KNN' :{'Classifier__n_neighbors': [1,5,10,25,50,100],'Classifier__weights': ['uniform','distance'],
                'Classifier__algorithm': ['auto','ball_tree','kd_tree']},
        'XGB':  {'Classifier__learning_rate': [0.2, 0.3, 0.5], 'Classifier__max_depth': [6, 9, 12],
                   'Classifier__subsample': [0.8, 1.0],  'Classifier__colsample_bytree': [0.8, 1.0]}

        }

    return clfs, grid

    from sklearn.metrics import log_loss, make_scorer


def magic_loop(models_to_run, clfs, grid, pipeline, df_train, df_test, search =1):
    type_labs = list(set(df_train.triptype.ravel()))
    y = df_train.triptype.ravel()
    X = df_train.drop(['triptype'], axis=1)


    for index, clf in enumerate([clfs[x] for x in models_to_run]):
        model_name = models_to_run[index]
        print(model_name)
        parameter_values = grid[models_to_run[index]]
        #parameter_values['PCA__n_components'] = [4,6,8,77] # [2,4,6,78]
        try:
            pipeline.set_params(Classifier = clf)
            if(search):
                gs = GridSearchCV(pipeline, parameter_values, cv=StratifiedKFold(2),
                                  n_jobs=-1, scoring='neg_log_loss', verbose= 1)
            else:
                gs = RandomizedSearchCV(pipeline, parameter_values, cv=StratifiedKFold(2),
                                         n_jobs=-1, scorer=sklearn.metrics.log_loss(labels=type_labs))
            start = time()

            gs.fit(X, y)

            print("GridSearch time: " + (str)(time() - start))

            ## Guardar el model en un archivo
            nom_pkl =str(model_name) +"_dic19_1644.pkl"
            addr= os.path.join(models,nom_pkl)
            with open(addr, "wb") as f:
                 pickle.dump(regr, f)

            nom_csv = "cv_"+ str(model_name) +"_19dic_1644.csv"
            addr= os.path.join(results, nom_csv)
            pd.DataFrame(gs.cv_results_).to_csv(addr)

            y_pred = gs.predict_proba(df_test)
            nom_csv = str(model_name) +"_dic19_1644"
            create_submit(y_pred, nom)

            print("Fin del magic_loop")

        except IndexError as e:
            print('Error:', e)
            continue



pipeline = Pipeline([
    ('VarianceThreshold', VarianceThreshold()),
    ('Normalizer', StandardScaler()),
    ('Classifier', RandomForestClassifier(n_estimators=10, n_jobs=-1))
    ])

models = ['XGB']

clfs, grid = define_hyper_params()
results_S = magic_loop( models, clfs, grid, pipeline, df_train,df_test)
