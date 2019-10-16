import json
import csv

def write_to_csv(option, data):
	print "\nWriting to csv file..."
	filename = ""
	headers = []

	if (option == 1):
		filename = "daily_pickups_min.csv"
		headers = ["date", "num_pickups", "num_minutes"]
	elif (option == 2):
		filename = "daily_session_min.csv"
		headers = ["date", "time", "session_min"]

	csv_file = open(filename, 'w')
	csv_writer = csv.writer(csv_file, delimiter=',')
	csv_writer.writerow(headers)
	
	for datum in data:
		csv_writer.writerow(datum)

	csv_file.close()

def parse_json(option):
	print "\nExtracting data from JSON file..."
	file = open("moment.json")
	json_string = file.read()
	file.close()
	json_obj = json.loads(json_string)
	data = []
	if (option == 1):
		days = json_obj['days']
		for day in days:
			date = day['date'].split('T')[0]
			num_pickups = day['pickupCount']
			num_minutes = day['minuteCount']
			data.append([date, num_pickups, num_minutes])
	elif (option == 2):
		days = json_obj['days']
		for day in days:
			sessions = day['sessions']
			date = day['date'].split('T')[0]
			for session in sessions:
				time = session['date'].split('T')[1].split("-")[0]
				session_min = session['lengthInMinutes']
				data.append([date, time, session_min])
	else:
		print "Unknown Option: [", option, "]"
	return data

def main():
	option = int(raw_input("Options:\n(1) {date, num_pickups, num_minutes}\n(2) {date, time, session_min}\n"))
	data = parse_json(option)
	if (len(data) > 0):
		write_to_csv(option, data)
	else:
		print "No data to write."
	print "Done!"


main()



