import pandas as pd
import string
import os
from playsound import playsound
import sys

class NumberReader:

    def __init__(self, dataframe):
        self.dataframe = dataframe
        self.mapping = {}
        
        for i, row in dataframe.iterrows():
            self.mapping[row.digit] = row.word
        
        self.tens = {}

        # fix mapping
        self.mapping[''] = ''
        self.tens_keys = ['', 'thousand', 'million', 'billion']

        for key in self.tens_keys:
            self.tens[key] = self.mapping[key]

    def process_chunk(self, string):
        hundreds = string[0]
        if hundreds == '0':
            hundred_string = ''
            tens = str(int(string[1:]))
            if tens == '0':
                return ''
            else:
                return self.mapping[tens]
        else:
            hundred_string = hundreds
            tens = str(int(string[1:]))
            if tens == '0':
                return self.mapping[hundred_string] + ' ' + self.mapping['hundred']
            else:
                return self.mapping[hundred_string] + ' ' + self.mapping['hundred'] + ' ' + self.mapping[tens]


    def read_whole_number(self, string):
        rev = string[::-1]
        chunks = []
        for i in range(0,len(rev),3):
            chunks.append(rev[i:i+3][::-1])
        chunks = chunks[::-1]
        result = ""
        tens_idx = len(chunks)-1
        
        for chunk in chunks:
            if len(chunk) < 3:
                result += self.mapping[chunk] + ' '
                result += self.tens[self.tens_keys[tens_idx]] + ' '
            elif chunk == '000':
                result = result
            else:
                result += self.process_chunk(chunk) + ' '
                result += self.tens[self.tens_keys[tens_idx]] + ' '
            tens_idx -= 1
        return result


    def read_all(self, string):
        res = ""
        for n in string:
            res += self.mapping[n] + ' '
        return res
    

    def read_number(self, string):
        if string.isnumeric() and 1500 < int(string) < 2100:
            if string[1:-1] == '00':
                return self.read_whole_number(string)
            elif string[-2:] == '00':
                return self.read_whole_number(string[:2]) + 'hundred'
            else:
                if string[2:][0] == '0':
                    return self.read_whole_number(string[:2]) + 'zero ' + self.read_whole_number(string[3:])
                else:
                    return self.read_whole_number(string[:2]) + self.read_whole_number(string[2:])
        elif self.mapping['currency_symbol'] in string:
            return self.read_number(string[1:]) + ' ' + self.mapping['currency'] + 's'
        elif '.' in string:
            if string[-1] == '.':
                return self.read_number(string[:-1])
            s1, s2 = string.split('.')
            return self.read_number(s1) + ' ' + self.mapping['.'] + ' ' + self.read_all(s2)
        elif ',' in string:
            string = string.replace(',','')
        elif ':' in string:
            s1, s2 = string.split(':')
            if int(s1) > 12:
                return self.read_whole_number(str(int(s1)-12)) + self.read_whole_number(s2) + ' PM'
            return self.read_whole_number(s1) + self.read_whole_number(s2) + ' AM'
        elif '-' in string:
            return self.mapping['-'] + self.read_number(string[1:])
        elif '+' in string: #phone number?
            return self.mapping['+'] + ' ' + self.read_all(string[1:])
        elif '%' in string:
            return self.read_number(string[:-1]) + ' ' + self.mapping['%']
        elif len(string) > 4:
            return self.read_all(string)
        else:
            return self.read_whole_number(string)


    def process_final(self, string):
        return ' '.join([x for x in self.read_number(string).split(' ') if x!= ''])


    def process_text(self, s):
        tokens = s.split(' ')
        tokens = [token for token in tokens if token not in string.punctuation]
        sentence = []
        for token in tokens:
            if any(i.isdigit() for i in token):
                sentence.append(self.process_final(token))
            else:
                sentence.append(token)
        return ' '.join(sentence)
    