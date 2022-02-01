import string

file = open("txt.done.data", "r")
read_data = file.read()
words = read_data.lower().translate(str.maketrans('', '', string.punctuation))
words = words.split()
unique_words = set(words)
unique_words = [word for word in unique_words if "time" not in word]

for word in unique_words:
    print("(lex.add.entry \'( \"{}\" nil (  ((CHARS) 0) )))".format(word))