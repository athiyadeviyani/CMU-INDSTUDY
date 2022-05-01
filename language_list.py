import pandas as pd

# SOURCE: https://github.com/festvox/datasets-CMU_Wilderness/blob/master/LangList.txt
# 1  LANGID six letter language id from bible.is
# 2  TLC    three letter language code (iso 639-3)
# 3  WIKI   Wikipedia link to language description
# 4  START  start url at bible.is
# 5  LAT    geolocated latitude
# 6  LONG   geolocated longitude
# 7  #utt0  Number utterances found in Pass 0 (cross-lingual alignment)
# 8  MCD0   Mel Cepstral Distortion score for Pass 0 (smaller is better)
# 9  #utt1  Number utterances found in Pass 1 (in-language alignment)
# 10 MCD1   Mel Cepstral Distortion score for Pass 1 (smaller is better)
# 11 Dur    HH:MM:SS duration of alignment data (from Pass 1)
# 12 MCDB   Mel Cepstral Distortion score for base CG synthesizer
# 13 MCDR   Mel Cepstral Distortion score for Random Forest CG synthesizer
# 14+ NAME  Text name of language (may be multiple fields)

def get_language_df():
    with open("LangList.txt", encoding='utf-8', mode='r') as file:
        all_lines = {}
        seen = {}
        columns = "LANGID TLC WIKI START LAT LONG #utt0 MCD0 #utt1 MCD1 Dur MCDB MCDR NAME".split()
        for line in file.readlines():
            split_data = line.split()
            tlc = split_data[1]
            mcdr = float(split_data[12])
            
            if tlc not in seen or seen[tlc] > mcdr:
                item = split_data[:13]
                item += [' '.join(split_data[13:])]
                all_lines[tlc] = item
                seen[tlc] = mcdr
    
            else:
                continue 

        df = pd.DataFrame(list(all_lines.values()), columns=columns)
        return df

# df = get_language_df()
# print(df)

# LANGID TLC WIKI START LAT LONG #utt0 MCD0 #utt1 MCD1 Dur MCDB MCDR NAME

