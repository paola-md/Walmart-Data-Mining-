import pickle
import numpy as np
from flask import Flask, jsonify, request
import pandas as pd
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import RandomForestClassifier
import sklearn.metrics 
import os


app = Flask(__name__)

## Cargarlo de nuevo
with open("RFPruned.pkl", "rb") as f:
     model = pickle.load(f)
    
@app.route('/modelo', methods=['POST']) 
def modelo(): 
    json = request.get_json()
    data = request.form if json is None else json
    upc = data['upc']
    day = data['day']
    count = int(data['count'])
    dep = int(data['dep'])
    finel = data['finel']
    
    trans = np.zeros((1, 77))
    trans[0, int(dep)] = 1 #69
    trans[0, 70] = int(count) #Obj absolutos
    trans[0, 71] = abs(int(count)) #Num objetos
    if count < 0:
        trans[0,72] = 1 #porc_devol
        trans[0,73] = 1 #devol
    if finel >0:
        trans[0,74] = int(count) #unique prod
    trans[0,75] = int(day)     
    if day > 5:
        trans[0,76] = 1 #weekend  

    results = {}
    y = model.predict(trans)
    resp = y[0].tolist()
    
    
    df_names = pd.read_csv('DepartmentDescription.csv')

    resp = df_names[df_names.id ==  int(resp)]
    final_name = resp.dep.tolist()
    results['visit_type'] = final_name
    return jsonify(results)

@app.route('/')
def api_root():
    return 'Servicio modelo vivo'

if __name__ == '__main__':
    app.run(host= '0.0.0.0') # , debug=True, port=5000
