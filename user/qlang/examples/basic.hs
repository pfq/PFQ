-- basic
--

main = filter is_ip >-> when is_tcp drop

