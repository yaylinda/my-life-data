import mailbox
import csv

csv_file = open('sent.csv', 'w')
csv_writer = csv.writer(csv_file, delimiter=',')
csv_writer.writerow(['date'])

mbox = mailbox.mbox('gmail.mbox')

for message in mbox.itervalues():
    dmessage = dict(message.items())
    if ('Date' in dmessage.keys() and 'X-Gmail-Labels' in dmessage.keys()):
    	if (dmessage['X-Gmail-Labels'] == 'Sent'):
    		csv_writer.writerow([dmessage['Date']])