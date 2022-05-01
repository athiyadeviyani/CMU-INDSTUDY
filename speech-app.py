import streamlit as st
import pandas as pd 
import numpy as np
import pandas as pd
import os
import base64
from number_reader import NumberReader
from language_list import get_language_df

LOCAL_DIR = "/Users/athiyadeviyani/CMU-INDSTUDY"
saved = False
st.write("""
# Number reader
""")

print(os.getcwd())
st.write("""
## Please select your language
""")

languages = get_language_df()

languages_list = []
lang_dict = {}

for i, row in languages.iterrows():
    lang_dict[row.TLC] = row.LANGID
    languages_list.append(row.NAME + " (" + row.TLC + ")")

language = st.selectbox("Language", languages_list + ["Other"])


other = st.text_input("If you selected other, please type your language below and hit ENTER or RETURN", value='')

if other != '':
    language = other

tlc = language.split('(')[1][:-1]
if tlc in lang_dict:
    lang_id = lang_dict[tlc]
    flitevox_code = 'cmu_' + lang_id[:3].lower() + "_" + lang_id[3:].lower() + '.flitevox'
else:
    lang_id = 'EN1NIV'
    flitevox_code = 'cmu_en1_niv.flitevox'

st.write("""
## Upload CSV

Please fill and upload a CSV file.
""")

def get_table_download_link_csv(df):
    csv = df.to_csv().encode()
    b64 = base64.b64encode(csv).decode()
    href = 'You can download the template <a href="data:file/csv;base64,{b64}" download="numbermap.csv" target="_blank">here</a>. Please add an additional column that maps a digit with its word-form.'
    return href

numbermap_template = pd.read_csv("numbermap_template.csv")
st.markdown(get_table_download_link_csv(numbermap_template), unsafe_allow_html=True)

st.write("Below is a preview of the template.")
st.write(numbermap_template.head())

def save_uploadedfile(uploadedfile, filename):
    with open(os.path.join("saved",filename),"wb") as f:
        f.write(uploadedfile.getbuffer())

mapping, tens = None, None
uploaded_file = st.file_uploader("Choose a file")
number_reader = None

if uploaded_file is not None:
    dataframe = pd.read_csv(uploaded_file)
    number_reader = NumberReader(dataframe)

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
        processed_text = ''.join([s.replace('\n', '...\n') for s in number_reader.process_text(number)])
        
        #print('Processed text:', processed_text)
        st.write("Original text: {}".format(number))
        st.write("Processed text: {}".format(processed_text))

        if os.getcwd() == LOCAL_DIR:
            syscall = "flite/bin/flite \"" + processed_text + "\" test.wav"
            os.system(syscall)

            audio_file = open('test.wav', 'rb')
            audio_bytes = audio_file.read()

            st.audio(audio_bytes, format="audio/wav", start_time=0)

        # Located on server
        else:
            voice_dir = "../datasets-CMU_Wilderness/allvoices/{}/voices/{}".format(lang_id, flitevox_code)
            syscall = "flite/bin/flite \"" + processed_text + "\" -voice " + voice_dir + " test.wav"
            os.system(syscall)

            audio_file = open('test.wav', 'rb')
            audio_bytes = audio_file.read()

            st.audio(audio_bytes, format="audio/wav", start_time=0)
    
    
    # "-voice /~" to the flitevox
    # /home/awb/datasets-CMU_Wilderness/allvoices
    # home/awb/datasets-CMU_Wilderness/allvoices/QEJLLB/cmu_qej_llb.flitevox
    # flite/bin/flite "hello" -voice ../datasets-CMU_Wilderness/allvoices/QEJLLB/voices/cmu_qej_llb.flitevox test.wav
    # flite/bin/flite "hello" test.wav

# playsound("tia.wav")

