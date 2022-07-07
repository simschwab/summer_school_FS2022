from flask import Flask, request, jsonify, session, url_for, redirect, render_template
import joblib

from parameters import Parameters

classifier_loaded = joblib.load("/Users/simschwab/dev/summer_school_FS2022/Credit_Risk_Modeling/flask_credit_risk_model/saved_models/model.pkl")
encoder_loaded = joblib.load("/Users/simschwab/dev/summer_school_FS2022/Credit_Risk_Modeling/flask_credit_risk_model/saved_models/encoder.pkl")

# Prediction function
def make_prediction(model, encoder, sample_json):
    # parse input from request
    Var1 = sample_json['Var1']
    Var2 = sample_json['Var2']
    Var3 = sample_json['Var3']
    Var4 = sample_json['Var4']

    # Make an input vector
    borrower = [[Var1, Var2, Var3, Var4]]

    # Predict
    prediction_raw = model.predict(borrower)

    # Convert Species index to Species name
    prediction_real = encoder.inverse_transform(prediction_raw)

    return prediction_real[0]

app = Flask(__name__)
app.config['SECRET_KEY'] = 'mysecretkey'

@app.route("/", methods=['GET','POST'])
def index():
    form = Parameters()

    if form.validate_on_submit():
        session['Var1'] = form.Var1.data
        session['Var2'] = form.Var2.data
        session['Var3'] = form.Var3.data
        session['Var4'] = form.Var4.data

        return redirect(url_for("prediction"))
        
    return render_template("home.html", form=form)

# Read models
classifier_loaded = joblib.load("/Users/simschwab/dev/summer_school_FS2022/Credit_Risk_Modeling/flask_credit_risk_model/saved_models/model.pkl")
encoder_loaded = joblib.load("/Users/simschwab/dev/summer_school_FS2022/Credit_Risk_Modeling/flask_credit_risk_model/saved_models/encoder.pkl")

@app.route('/prediction')
def prediction():
    content = {'Var1': float(session['Var1']), 'Var2': float(session['Var2']),
               'Var3': float(session['Var3']), 'Var4': float(session['Var4'])}

    results = make_prediction(classifier_loaded, encoder_loaded, content)

    return render_template('prediction.html', results=results)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8080)
