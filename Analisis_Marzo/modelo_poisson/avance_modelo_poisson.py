# %% [markdown]
# # Introducción
# 
# Se presenta el reporte detallado que corresponde al mes de marzo, este documento tiene como objetivo aplicar técnicas estadísticas avanzadas para analizar los datos de graduados y otras variables relevantes como sector, metodología, área de conocimiento, semestre de la IES y otras variables que distinguen aun más los grupos como el sexo, año para tener unos conteos que son demanda real (inscritos), admitidos, demanda potencial (graduados).
# 
# Siguiendo con la idea de los otros informes contamos con la agrupación de las bases de datos donde se eliminan variables que se cree que no aportan a la creación de modelos de predicción como el conteo de los matriculados, como el nombre de la institución (demasiados nombres que afectarían el modelo).
# 
# El tratamiento de los datos y la creación de los modelos respectivos está hecha en python.
# 
# 
# # Lectura de los datos
# 
# ## Librerias necesarias

# %%
import pandas as pd
from matplotlib import pyplot as plt
import numpy as np
import seaborn as sns
from patsy import dmatrices
import statsmodels.api as sm
import warnings
plt.style.use('ggplot')

# %% [markdown]
# Se leen los datos de la agrupación total que contiene:
# 
# - n° de inscritos
# - n° de admitidos
# - n° de graduados

# %%
ruta = "/home/daniel/OneDrive/Documentos (OneDrive-UNAL)/pro_inv/Proyecto-de-Investigacion/Analisis_Marzo/BD_agrupada.xlsx"

df0 = pd.read_excel(ruta)
df0['ano'] = df0['ano'].astype("category")

# %% [markdown]
# Antes de analizar los datos, necesitamos observar que el conteo a utilizar como variable de respuesta $Y$ cumplan con el supuesto que la media (nuestro parametro $\lambda$) sea constante, es decir que los valores varien frente a una valor

# %%
# Columnas a cosiderar
v = list()
for i in [0,1,2,3,5,6,7,8,9]:
    v.append(list(df0.columns.values)[i])

# Nueva BD agrupando por nyear
df = df0[v].groupby(['ano', 'sector_ies', 'sexo', 'area_de_conocimiento'], as_index=False)[v[6:9]].agg(np.sum)

# %% [markdown]
# Una vez se agruparon los datos por las variables pertinetes tenemos que nuestra nueva BD es:

# %%
df.head(5)

# %% [markdown]
# Ahora se precede a graficar los datos del conteo de la variable de respuesta (demanda potencial) por algún tipo de varible a agrupar, en este caso se toma por sexo según el caracter de la IES.

# %%
# Serie de tiempo de los datos de Demanda potencial
ax = sns.relplot(kind='line', data=df, x = 'ano', y = 'demanda_potencial', hue='sexo',col='sector_ies', errorbar=None)
ax.tick_params(axis='x', rotation=45)
plt.show()

# %% [markdown]
# Se puede notar que en los dos casos ninguno cuenta con media constante.
# 
# # Partición de los datos
# 
# se crean los datos de entramiento y test de los datos, teniendo en cuenta que estos se parten de forma secuencial, se toma el 80% de los datos de arriba hacia abajo dejando el 20% al conjunto de testeo.
# 

# %%
df_train = df.iloc[0:384,:]
df_test = df.iloc[384:480,:]
print('Training data set length='+str(len(df_train)))
print('Testing data set length='+str(len(df_test)))

# %% [markdown]
# Ahora creamos la formula para aplicar al modelo:

# %%
expr = "demanda_potencial~ano+sector_ies+sexo+area_de_conocimiento+demanda_real+admitidos"

# One-hot encoding
y_train, X_train = dmatrices(expr, df_train,return_type='dataframe')
y_test, X_test = dmatrices(expr, df_test, return_type='dataframe')