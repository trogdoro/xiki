# return "127.3.3.7"   # Hard-coded IP, for when demoing

# https://stackoverflow.com/a/7809304

ip_addr = Socket.ip_address_list.detect { |ip| ip.ipv4_private? }
return "| #{ip_addr.ip_address}"
