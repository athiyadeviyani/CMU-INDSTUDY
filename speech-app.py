from copyreg import pickle
import streamlit as st
import pandas as pd 
import numpy as np
import pandas as pd
import string
import os
from playsound import playsound
import sys
import base64
from number_reader import process_text, set_mapping

saved = False
st.write("""
# Number reader
""")

st.write("""
## Please select your language
""")

languages = pd.read_csv("languages.csv")
languages_list = []

for i, row in languages.iterrows():
    languages_list.append(row.Language + "_" + row.Code)

language = st.selectbox("Language", languages_list + ["Other"])


other = st.text_input("If you selected other, please type your language below and hit ENTER or RETURN", value='')

if other != '':
    language = other

st.write("""
## Upload CSV

Please fill and upload a CSV file.
""")

def get_table_download_link_csv(df):
    #csv = df.to_csv(index=False)
    csv = df.to_csv().encode()
    #b64 = base64.b64encode(csv.encode()).decode() 
    b64 = base64.b64encode(csv).decode()
    href = f'You can download the template <a href="data:file/csv;base64,{b64}" download="numbermap.csv" target="_blank">here</a>. Please add an additional column that maps a digit with its word-form.'
    return href

numbermap_template = pd.read_csv("numbermap_template.csv")
st.markdown(get_table_download_link_csv(numbermap_template), unsafe_allow_html=True)

st.write("Below is a preview of the template.")
st.write(numbermap_template.head())

def save_uploadedfile(uploadedfile, filename):
    with open(os.path.join("saved",filename),"wb") as f:
        f.write(uploadedfile.getbuffer())
    #  return st.success("Saved File:{} to saved".format(uploadedfile.name))

mapping, tens = None, None
uploaded_file = st.file_uploader("Choose a file")
if uploaded_file is not None:
    dataframe = pd.read_csv(uploaded_file)
    mapping, tens = set_mapping(dataframe)


    st.write("Below is a preview of your uploaded file.")
    st.write(dataframe.head())
    random_id = np.random.randint(1000)
    filename = language + "_" + str(random_id)
    if not saved:
        save_uploadedfile(uploaded_file, filename)
        saved = True





st.write("""
## Read out number

Please type a numeric string to be read-out. The numbers can be a combination of any of the following formats:
 - whole numbers: 1235523
 - phone numbers: +4120987654
 - currency: $23
 - floating-point numbers: 21.25
 - time: 19:10
 - percentage: 90%
""")

number = st.text_input("Please type your number here and hit ENTER or RETURN", value="")

if number != "":
    #print('Original text:', number)
    processed_text = ''.join([s.replace('\n', '...\n') for s in process_text(number)])
    
    #print('Processed text:', processed_text)
    st.write("Original text: {}".format(number))
    st.write("Processed text: {}".format(processed_text))
    syscall = "flite/bin/flite \"" + processed_text + "\" test.wav"

    os.system(syscall)

    audio_file = open('test.wav', 'rb')
    audio_bytes = audio_file.read()

    st.audio(audio_bytes, format="audio/wav", start_time=0)

# playsound("tia.wav")

