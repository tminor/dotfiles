#!/bin/bash

calendar_dir='/home/tminor/org/calendar'
workcal_url='https://outlook.office365.com/owa/calendar/42e4a48bfdef446dbd1fa75c179bf353@usg.edu/506aabb7cb6644fa92df0458ac1a26ac4263737238637684450/calendar.ics'
homecal_url='https://calendar.google.com/calendar/ical/minort%40gmail.com/private-d5d0e0ccc6bfa0eb8007c958a264b3b4/basic.ics'
oncallcal_url='https://borusg.pagerduty.com/private/2821f0aca666c1fb0837751560391e7dfbd78dbcddef88b29691888523ada103/feed'

# Removes old .ics and fetches a more recent one.
rm $calendar_dir/*cal.ics*
rm $calendar_dir/*cal.org
wget $workcal_url -O $calendar_dir/workcal.ics
wget $homecal_url -O $calendar_dir/homecal.ics
wget $oncallcal_url -O $calendar_dir/oncallcal.ics

echo "$(cat ${calendar_dir}/homecal.orgheader)" >> $calendar_dir/workcal.org
echo "$(/home/tminor/src/ical2org/ical2org.rb -i ${calendar_dir}/workcal.ics)" >> $calendar_dir/workcal.org
echo "$(cat ${calendar_dir}/homecal.orgheader)" >> $calendar_dir/homecal.org
echo "$(/home/tminor/src/ical2org/ical2org.rb -i ${calendar_dir}/homecal.ics)" >> $calendar_dir/homecal.org
echo "$(cat ${calendar_dir}/homecal.orgheader)" >> $calendar_dir/oncallcal.org
echo "$(/home/tminor/src/ical2org/ical2org.rb -i ${calendar_dir}/oncallcal.ics)" >> $calendar_dir/oncallcal.org
