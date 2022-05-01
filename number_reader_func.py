import numpy as np
import pandas as pd
import string
import os
from playsound import playsound
import sys


mapping = None
tens = {}

mapping = pd.read_csv("numbermap.csv", header=None, index_col=0, squeeze=True).to_dict()
# mapping = mapping.to_dict()
# mapping = mapping[1]
mapping[''] = ''
# mapping['%'] = 'percent'

tens_keys = ['', 'thousand', 'million', 'billion']
# tens = {}
for key in tens_keys:
    tens[key] = mapping[key]

def set_mapping(df):
# Read number mapping from CSV
    # mapping = pd.read_csv(filename, header=None, index_col=0, squeeze=True).to_dict()
    # mapping = mapping.to_dict()
    # mapping = mapping[1]
    # mapping[''] = ''
    # # mapping['%'] = 'percent'
    # print(mapping)
    # print(df)

    mapping = {}
    
    for i, row in df.iterrows():
        mapping[row.digit] = row.word

    mapping[''] = ''

    print(mapping)
    tens_keys = ['', 'thousand', 'million', 'billion']
    # tens = {}
    for key in tens_keys:
        tens[key] = mapping[key]

    return mapping, tens

# tens = {
# # '100': 'hundred',
#  '': '',
#  '1000': 'thousand',
# #  '10000': 'ten thousand',
# #  '100000': 'hundred thousand',
#  '1000000': 'million',
# #  '10000000': 'ten million',
# #  '100000000': 'hundred million',
#  '1000000000': 'billion'}

# tens_keys = list(tens.keys())
# print(tens)

def read_whole_number(string):
    rev = string[::-1]
    chunks = []
    for i in range(0,len(rev),3):
        chunks.append(rev[i:i+3][::-1])
    chunks = chunks[::-1]
    result = ""
    tens_idx = len(chunks)-1
    
    for chunk in chunks:
        if len(chunk) < 3:
            result += mapping[chunk] + ' '
            result += tens[tens_keys[tens_idx]] + ' '
        elif chunk == '000':
            result = result
        else:
            result += process_chunk(chunk) + ' '
            result += tens[tens_keys[tens_idx]] + ' '
        tens_idx -= 1
    return result
    
def process_chunk(string):
    hundreds = string[0]
    if hundreds == '0':
        hundred_string = ''
        tens = str(int(string[1:]))
        if tens == '0':
            return ''
        else:
            return mapping[tens]
    else:
        hundred_string = hundreds
        tens = str(int(string[1:]))
        if tens == '0':
            return mapping[hundred_string] + ' ' + mapping['hundred']
        else:
            return mapping[hundred_string] + ' ' + mapping['hundred'] + ' ' + mapping[tens]
    

def read_all(string):
    res = ""
    for n in string:
        res += mapping[n] + ' '
    return res

def read_number(string):
    if string.isnumeric() and 1500 < int(string) < 2100:
        if string[1:-1] == '00':
            return read_whole_number(string)
        elif string[-2:] == '00':
            return read_whole_number(string[:2]) + 'hundred'
        else:
            if string[2:][0] == '0':
                return read_whole_number(string[:2]) + 'zero ' + read_whole_number(string[3:])
            else:
                return read_whole_number(string[:2]) + read_whole_number(string[2:])
    elif mapping['currency_symbol'] in string:
        return read_number(string[1:]) + ' ' + mapping['currency'] + 's'
    elif '.' in string:
        if string[-1] == '.':
            return read_number(string[:-1])
        s1, s2 = string.split('.')
        return read_number(s1) + ' ' + mapping['.'] + ' ' + read_all(s2)
    elif ',' in string:
        string = string.replace(',','')
    elif ':' in string:
        s1, s2 = string.split(':')
        if int(s1) > 12:
            return read_whole_number(str(int(s1)-12)) + read_whole_number(s2) + ' PM'
        return read_whole_number(s1) + read_whole_number(s2) + ' AM'
    elif '-' in string:
        return mapping['-'] + read_number(string[1:])
    elif '+' in string: #phone number?
        return mapping['+'] + ' ' + read_all(string[1:])
    elif '%' in string:
        return read_number(string[:-1]) + ' ' + mapping['%']
    elif len(string) > 4:
        return read_all(string)
    else:
        return read_whole_number(string)

def process_final(string):
    return ' '.join([x for x in read_number(string).split(' ') if x!= ''])

def process_text(s):
    tokens = s.split(' ')
    tokens = [token for token in tokens if token not in string.punctuation]
    sentence = []
    for token in tokens:
        if any(i.isdigit() for i in token):
            sentence.append(process_final(token))
        else:
            sentence.append(token)
    return ' '.join(sentence)

# Test year

# for _ in range(10):
#     n = np.random.randint(1500, 2100)
#     print(n, process_final(str(n)))
if len(sys.argv) > 1:
    text = sys.argv[1]

else:
    text = 'Athiya Deviyani is 22 years old and is born in May 3 1999. \n\
        Her phone number is +14127730373. \n\
        She lives in 4500 Centre Avenue, P.A. 15213. \n\
        She is 160.5 centimeters tall. \n\
        Her phone battery is at 90%. \n\
        She would like $11.95 to buy dinner at 19:35. \n' 

# text = 'dua ratus lima puluh enam'

print('Original text:', text)
processed_text = ''.join([s.replace('\n', '...\n') for s in process_text(text)])
print('Processed text:', processed_text)
syscall = "flite/bin/flite \"" + processed_text + "\" tia.wav"
print(syscall)
os.system(syscall)

# filename = 'tia.wav'
# wave_obj = sa.WaveObject.from_wave_file(filename)
# play_obj = wave_obj.play()
# play_obj.wait_done()  # Wait until sound has finished playing

playsound("tia.wav")
