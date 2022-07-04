from flask_wtf import FlaskForm
from wtforms import StringField, SubmitField

class Parameters(FlaskForm):
    Var1 = StringField("Var1")
    Var2 = StringField("Var2")
    Var3 = StringField("Var3")
    Var4 = StringField("Var4")

    submit = SubmitField("Predict")
