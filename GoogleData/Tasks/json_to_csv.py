import json
import csv

file = open('Tasks.json', 'r')
json_data = file.read()
json_obj = json.loads(json_data)
task_json_list = json_obj[u'items'][0][u'items']

csv_file = open('data.csv', 'w')
csv_writer = csv.writer(csv_file, delimiter=',')
csv_writer.writerow(['title', 'date'])

for i in range(0, len(task_json_list)):
	if (u'due' in task_json_list[i].keys()):
		csv_writer.writerow([task_json_list[i][u'title'], task_json_list[i][u'due']])