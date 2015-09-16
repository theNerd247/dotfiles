##############################
# dzenchkmail.sh
#
# author: Noah Harvey
# version: 0.0.1
# 
# checks for new mail
##############################

new=`ls -1 $MAIL/new/ | wc -l`

echo "Inbox: $new"
