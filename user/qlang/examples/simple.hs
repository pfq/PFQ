-- example
--

filter_function = par ip ip6

main =
    steer_flow >->
    filter_function >-> when is_tcp drop >-> kernel

