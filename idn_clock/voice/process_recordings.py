import os
from pydub import AudioSegment

sound_files = os.listdir()  # format: "[Carnegie Mellon University 2.m4a]"

for sound_file in sound_files:
    if ".py" not in sound_file and sound_file != ".DS_Store":
        print(sound_file)
        digit = sound_file.split(' ')[-1].split('.')[0]
        if len(digit) == 1:
            digit = '0' + digit
        track = AudioSegment.from_file(sound_file, format="m4a").set_frame_rate(16000)
        wav_filename = "../recording/time_00" + digit + ".wav"
        file_handle = track.export(wav_filename, format="wav")

