import csv

csv_file = open('times.csv', 'w')
csv_writer = csv.writer(csv_file, delimiter=',')
csv_writer.writerow(['date', 'type', 'time'])

file = open('raw.txt', 'r')
lines = file.readlines()
lines = [x.strip() for x in lines]
file.close()

for line in lines:
	date = line.split(' ')[0]
	# print date

	kind = line.split(' ')[1].split(' ')[0]
	# print kind

	time = ''
	if ':' in line:
		time = line.split('- ')[1]
	# print time

	csv_writer.writerow([date, kind, time])

csv_file.close()