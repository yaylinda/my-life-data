import requests
import json
import csv
 
url_template = 'https://api.foursquare.com/v2/users/self/checkins?limit=250&oauth_token={}&v=20170901&offset={}'
oauth_token = "IYDQJB4TGCNV1J0ZV4O2NG24MMZZWHPTYROQCRLHPIHYAGFW"
offset = 0
 
csv_file = open('swarm.csv', 'w')
csv_writer = csv.writer(csv_file, delimiter=',')
csv_writer.writerow(['timestamp', 'offset'])

while True:

	url = url_template.format(oauth_token, offset)
	print 'requesting... ' + url

	response = requests.get(url)

	if 'checkins' in response.json()['response']:
		if len(response.json()['response']['checkins']['items']) == 0:
			break

		items = response.json()['response']['checkins']['items']

		for item in items:
			timestamp = item['createdAt']
			tz_offset = item['timeZoneOffset']
			csv_writer.writerow([timestamp, tz_offset])

	offset += 250