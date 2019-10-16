import sys
import mailbox
import datetime
import pytz
from pytz import timezone
import csv
import re

def mbox_to_csv(mbox_path, csv_filename, box) :
	csvfile = open(csv_filename + '.csv', 'w')
	fieldnames = ['year', 'month', 'day', 'dow', 'hour', 'minute']
	writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
	writer.writeheader()

	mbox = mailbox.mbox(mbox_path)

	for i in range(0, len(mbox)):
		# print "*************************************"
		string_to_search = ""
		if (box == "i"):
			string_to_search = mbox[i]['Received'].replace('\n', '')
		else:
			string_to_search = mbox[i]['date']
		# print string_to_search
		pattern = re.compile(("[0-9]+ [A-Z|a-z]{3} [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2}"))
		matched = pattern.findall(string_to_search)
		date_str = matched[0]

		utc_date_obj = pytz.utc.localize(datetime.datetime.strptime(date_str, '%d %b %Y %H:%M:%S'))

		central_tz = timezone('US/Central')
		date_obj = utc_date_obj.astimezone(central_tz)

		data_dict = {}
		data_dict['year'] = date_obj.year
		data_dict['month'] = date_obj.month
		data_dict['day'] = date_obj.day
		data_dict['dow'] = date_obj.weekday()
		data_dict['hour'] = date_obj.hour
		data_dict['minute'] = date_obj.minute

		writer.writerow(data_dict)

	return i

if __name__ == "__main__":
	if ("help" in sys.argv):
		print "Usage: python mbox_to_csv.py <path_to_mbox_file> <csv_filename> <box_type>"
	else:
		print "\npath to mbox file: " + sys.argv[1]
		print "csv filename: " + sys.argv[2]
		print "box type: " + sys.argv[3]
		print "\ndoing mbox to csv data extraction..."
		total = mbox_to_csv(sys.argv[1], sys.argv[2], sys.argv[3])
		print "\ndone! extrated " + str(total) + " messages."
