
import os
import sys
import re
import codecs

def processFile(filepath):
    fp = codecs.open(filepath, 'rU', 'iso-8859-2')

    content = fp.read()

#
#  INSERT YOUR CODE HERE
#  -->

    authorPattern = re.compile(r'<META NAME="AUTOR" CONTENT="(.*[^\\])">?')
    author = authorPattern.search(content).group(1)

    sectionPattern = re.compile(r'<META NAME="DZIAL" CONTENT="(.*[^\\])">?')
    section = sectionPattern.search(content).group(1)

    keywordsPattern = re.compile(r'<META NAME="KLUCZOWE_?\d*" CONTENT="(.*[^\\])">?')
    keywords = [str(keyword) for keyword in re.findall(keywordsPattern, content)]

    textPattern = re.compile(r'<P>.*</P>' ,re.DOTALL)
    text = textPattern.search(content).group(0)
 
    textPattern = re.compile(r'<.*?>' )
    text = re.sub(textPattern,'',text)
	
	# emailPattern = re.compile(r'(([^\s\n\.@]+\.?)+@[^\s\n\.@]+\.([^\s\n\.@]+\.?)+[^\s\n\.@]+)')
    emailPattern = re.compile(r'(([\w_-]+\.?)+@[\w_-]+\.([\w_-]+\.?)+\w+)')
    emails = [str(x[0]) for x in re.findall(emailPattern, text)]
    for email in emails:
        text.replace(email, '')
    emails = len(set(emails))

    shortcutPattern = re.compile(r'( [A-Za-z]{,3}?\.)')
    shortcut = len(re.findall(shortcutPattern,text))
	
    shortcutPattern = re.compile(r'( [A-Za-z]{,3}?\.)(?!\s[A-Z])')
    text = re.sub(shortcutPattern,' ',text)

    decPattern = re.compile(r' (3276[0-7]|327[0-5][0-9]|32[0-6][0-9][0-9]|3[01][0-9][0-9][0-9]|[12][012][0-9][0-9][0-9]|[1-9]?[0-9]{,3}|-(3276[0-8]|327[0-5][0-9]|32[0-6][0-9][0-9]|3[01][0-9][0-9][0-9]|[12][012][0-9][0-9][0-9]|[1-9]?[0-9]{,3})) ')
    dec = len(set(re.findall(decPattern,text)))

    floatPattern = re.compile(r'[^\d\.]((-?\d+\.\d*(e(\+?|-)\d+)?)|(-?\d*\.\d+(e(\+?|-)\d+)?))[^\d\.]')
    floats = [str(x[0]) for x in re.findall(floatPattern, text)]
    floats = len(set(floats))

    sentencesPattern = re.compile(r'[A-Z].*?([\.!?]+|\n)',re.DOTALL)
    sentences = len(re.findall(sentencesPattern,text))
    subPattern = re.compile(r'[-/]')
    # text = re.sub(subPattern,'.',text)

    datesPattern = re.compile(r'(?:(?:(?:\d{4}/)?(?:(?:(?:[0-2][0-9]|3[01])/(?:0[13578]|1[02]))|(?:(?:[0-2][0-9]|30)/(?:0[469]|11))|(?:[0-2][0-9]/02))(?:/\d{4})?)|(?:(?:\d{4}-)?(?:(?:(?:[0-2][0-9]|3[01])-(?:0[13578]|1[02]))|(?:(?:[0-2][0-9]|30)-(?:0[469]|11))|(?:[0-2][0-9]-02))(?:-\d{4})?)|(?:(?:\d{4}\.)?(?:(?:(?:[0-2][0-9]|3[01])\.(?:0[13578]|1[02]))|(?:(?:[0-2][0-9]|30)\.(?:0[469]|11))|(?:[0-2][0-9]\.02))(?:\.\d{4})?))')
    dates = re.findall(datesPattern,text)
    yearPattern = re.compile(r'(?:\d{4}\.)|(?:\.\d{4})')
    s = set()
    for date in dates:
        d = re.sub(subPattern,'.',date)
        y = yearPattern.search(d)
        d = re.sub(yearPattern,'',date)
        d = str(d) + str(y)
        d = re.sub(r'\.','',date)
        s.add(date)



    fp.close()
    print("nazwa pliku: " + filepath)
    print("autor: " + author)
    print("dzial: " + section)
    print("slowa kluczowe: " + str(keywords))
    print("liczba zdan: " + str(sentences))
    print("liczba skrotow: " + str(shortcut))
    print("liczba liczb calkowitych z zakresu int: " + str(dec))
    print("liczba liczb zmiennoprzecinkowych: " + str(floats))
    print("liczba dat: " + str(len(s)))
    print("liczba adresow email: " + str(emails))
    print("\n")



try:
    path = sys.argv[1]
except IndexError:
    print("Brak podanej nazwy katalogu")
    sys.exit(0)


tree = os.walk(path)

for root, dirs, files in tree:
    for f in files:
        if f.endswith(".html"):
            filepath = os.path.join(root, f)
            processFile(filepath)


