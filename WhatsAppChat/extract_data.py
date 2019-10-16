import csv

def write_to_csv(data):
	print "\nWriting to csv file..."
	csv_file = open('data.csv', 'w')
	csv_writer = csv.writer(csv_file, delimiter=',')
	csv_writer.writerow(['date', 'time', 'name', 'text', 'category', 'count_sent', 'count_words'])
	
	for datum in data:
		csv_writer.writerow([
			datum['date'], 
			datum['time'], 
			datum['name'], 
			datum['text'], 
			datum['category'], 
			datum['count_sent'], 
			datum['count_words']])

	csv_file.close()

def categorize(text):
	category = "MESSAGE"
	# print "categorizing text: \"" + text + "\""

	if (text.find("www.reddit.com") > -1):
		category =  "REDDIT"
	elif (text.find("image omitted>") > -1):
		category =  "IMAGE"
	elif (text.find("https://") > -1) or (text.find("http://") > -1):
		category = "OTHER LINK"

	# print "CATEGORY: " + category
	return category

LINDA_LINE = ": Linda Zheng: "
SEAN_LINE = ": Sean Reardon: "
DATE_SPLIT = ", "
TIME_SPLIT = ": "

data = []
count = 0

file = open("_chat.txt")
content = file.readlines()
content = [x.strip() for x in content]

for line in content:
	if (line.find(LINDA_LINE) > -1 or line.find(SEAN_LINE) > -1):
		date = line.split(DATE_SPLIT)[0]
		date_parts = date.split("/")
		if (len(date_parts[0]) == 1):
			date_parts[0] = "0" + date_parts[0]
		if (len(date_parts[1]) == 1):
			date_parts[1] = "0" + date_parts[1]
		date_parts[2] = "20" + date_parts[2]
		date = date_parts[2] + "-" + date_parts[0] + "-" + date_parts[1]

		time =  line.split(TIME_SPLIT)[0].split(DATE_SPLIT)[1]
		isPm = False
		if (time.find("PM") > -1):
			isPm = True
		time_parts = time.split(":")
		hour = int(time_parts[0])

		if (isPm and hour != 12):
			hour = hour + 12
		elif ((not isPm) and hour == 12):
			hour = 0

		# time = str(hour) + ":" + time_parts[1] + ":00"
		time = str(hour)

		name = ""
		text = ""
		if (line.find(LINDA_LINE) > -1):
			name = "Linda"
			text = line.split(LINDA_LINE)[1]
		elif (line.find(SEAN_LINE) > -1):
			name = "Sean"
			text = line.split(SEAN_LINE)[1]

		if (len(text) > 0):
			category = categorize(text)
			datum = {}
			datum["date"] = date
			datum["time"] = time
			datum["name"] = name
			datum["text"] = text.lower()
			datum["category"] = category
			datum["count_sent"] = 1
			datum["count_words"] = len(text.split())
			data.append(datum)
			count = count + 1

print len(data)
write_to_csv(data)



