-- regression test...


predicates =
    is_ip                               .||.
    is_udp                              .||.
    is_tcp                              .||.
    is_icmp                             .||.
    is_flow                             .||.
    is_l3_proto 42                      .||.
    is_l4_proto 8                       .||.
    is_frag                             .||.
    is_first_frag                       .||.
    is_more_frag                        .||.
    is_rtp                              .||.
    is_rtcp                             .||.
    is_sip                              .||.
    is_voip                             .||.
    has_port 1024                       .||.
    has_src_port 21                     .||.
    has_dst_port 80                     .||.
    has_addr "192.168.0.0/24"           .||.
    has_addr (CIDR ("192.168.0.0", 24)) .||.
    has_dst_addr "10.0.0.0/16"          .||.
    has_state 45                        .||.
    has_mark 11                         .||.
    has_vlan                            .||.
    has_vid 42                          .||.
    vlan_id [1, 12]


combinators = is_ip .||. is_ip .&&. (not is_tcp .^^. is_udp)


combinators2 = par ip (inv udp)


comparators =
    (ip_tos      .<  42)  .||.
    (ip_tot_len  .<  42)  .||.
    (ip_id       .<  42)  .||.
    (ip_frag     .<  42)  .||.
    (ip_ttl      .<  42)  .||.
    (get_mark    .<  42)  .||.
    (get_state   .<  42)  .||.
    (tcp_source  .<  42)  .||.
    (tcp_dest    .<  42)  .||.
    (tcp_hdrlen  .<  42)  .||.
    (udp_source  .<  42)  .||.
    (udp_dest    .<  42)  .||.
    (udp_len     .<  42)  .||.
    (icmp_type   .<  42)  .||.
    (icmp_code   .<  42)

comparators2 =
    (ip_tos      .<=  42)  .||.
    (ip_tot_len  .<=  42)  .||.
    (ip_id       .<=  42)  .||.
    (ip_frag     .<=  42)  .||.
    (ip_ttl      .<=  42)  .||.
    (get_mark    .<=  42)  .||.
    (get_state   .<=  42)  .||.
    (tcp_source  .<=  42)  .||.
    (tcp_dest    .<=  42)  .||.
    (tcp_hdrlen  .<=  42)  .||.
    (udp_source  .<=  42)  .||.
    (udp_dest    .<=  42)  .||.
    (udp_len     .<=  42)  .||.
    (icmp_type   .<=  42)  .||.
    (icmp_code   .<=  42)

comparators3 =
    (ip_tos      .==  42)  .||.
    (ip_tot_len  .==  42)  .||.
    (ip_id       .==  42)  .||.
    (ip_frag     .==  42)  .||.
    (ip_ttl      .==  42)  .||.
    (get_mark    .==  42)  .||.
    (get_state   .==  42)  .||.
    (tcp_source  .==  42)  .||.
    (tcp_dest    .==  42)  .||.
    (tcp_hdrlen  .==  42)  .||.
    (udp_source  .==  42)  .||.
    (udp_dest    .==  42)  .||.
    (udp_len     .==  42)  .||.
    (icmp_type   .==  42)  .||.
    (icmp_code   .==  42)

comparators4 =
    (ip_tos      ./=  42)  .||.
    (ip_tot_len  ./=  42)  .||.
    (ip_id       ./=  42)  .||.
    (ip_frag     ./=  42)  .||.
    (ip_ttl      ./=  42)  .||.
    (get_mark    ./=  42)  .||.
    (get_state   ./=  42)  .||.
    (tcp_source  ./=  42)  .||.
    (tcp_dest    ./=  42)  .||.
    (tcp_hdrlen  ./=  42)  .||.
    (udp_source  ./=  42)  .||.
    (udp_dest    ./=  42)  .||.
    (udp_len     ./=  42)  .||.
    (icmp_type   ./=  42)  .||.
    (icmp_code   ./=  42)

comparators5 =
    (ip_tos      .> 42)  .||.
    (ip_tot_len  .> 42)  .||.
    (ip_id       .> 42)  .||.
    (ip_frag     .> 42)  .||.
    (ip_ttl      .> 42)  .||.
    (get_mark    .> 42)  .||.
    (get_state   .> 42)  .||.
    (tcp_source  .> 42)  .||.
    (tcp_dest    .> 42)  .||.
    (tcp_hdrlen  .> 42)  .||.
    (udp_source  .> 42)  .||.
    (udp_dest    .> 42)  .||.
    (udp_len     .> 42)  .||.
    (icmp_type   .> 42)  .||.
    (icmp_code   .> 42)

comparators6 =
    (ip_tos      .>= 42)  .||.
    (ip_tot_len  .>= 42)  .||.
    (ip_id       .>= 42)  .||.
    (ip_frag     .>= 42)  .||.
    (ip_ttl      .>= 42)  .||.
    (get_mark    .>= 42)  .||.
    (get_state   .>= 42)  .||.
    (tcp_source  .>= 42)  .||.
    (tcp_dest    .>= 42)  .||.
    (tcp_hdrlen  .>= 42)  .||.
    (udp_source  .>= 42)  .||.
    (udp_dest    .>= 42)  .||.
    (udp_len     .>= 42)  .||.
    (icmp_type   .>= 42)  .||.
    (icmp_code   .>= 42)


comparators7 =
    (42 <. ip_tos      )  .||.
    (42 <. ip_tot_len  )  .||.
    (42 <. ip_id       )  .||.
    (42 <. ip_frag     )  .||.
    (42 <. ip_ttl      )  .||.
    (42 <. get_mark    )  .||.
    (42 <. get_state   )  .||.
    (42 <. tcp_source  )  .||.
    (42 <. tcp_dest    )  .||.
    (42 <. tcp_hdrlen  )  .||.
    (42 <. udp_source  )  .||.
    (42 <. udp_dest    )  .||.
    (42 <. udp_len     )  .||.
    (42 <. icmp_type   )  .||.
    (42 <. icmp_code   )

comparators8 =
    (42 <=. ip_tos      )  .||.
    (42 <=. ip_tot_len  )  .||.
    (42 <=. ip_id       )  .||.
    (42 <=. ip_frag     )  .||.
    (42 <=. ip_ttl      )  .||.
    (42 <=. get_mark    )  .||.
    (42 <=. get_state   )  .||.
    (42 <=. tcp_source  )  .||.
    (42 <=. tcp_dest    )  .||.
    (42 <=. tcp_hdrlen  )  .||.
    (42 <=. udp_source  )  .||.
    (42 <=. udp_dest    )  .||.
    (42 <=. udp_len     )  .||.
    (42 <=. icmp_type   )  .||.
    (42 <=. icmp_code   )

comparators9 =
    (42 ==. ip_tos     )  .||.
    (42 ==. ip_tot_len )  .||.
    (42 ==. ip_id      )  .||.
    (42 ==. ip_frag    )  .||.
    (42 ==. ip_ttl     )  .||.
    (42 ==. get_mark   )  .||.
    (42 ==. get_state  )  .||.
    (42 ==. tcp_source )  .||.
    (42 ==. tcp_dest   )  .||.
    (42 ==. tcp_hdrlen )  .||.
    (42 ==. udp_source )  .||.
    (42 ==. udp_dest   )  .||.
    (42 ==. udp_len    )  .||.
    (42 ==. icmp_type  )  .||.
    (42 ==. icmp_code  )

comparators10 =
    (42 /=. ip_tos     )  .||.
    (42 /=. ip_tot_len )  .||.
    (42 /=. ip_id      )  .||.
    (42 /=. ip_frag    )  .||.
    (42 /=. ip_ttl     )  .||.
    (42 /=. get_mark   )  .||.
    (42 /=. get_state  )  .||.
    (42 /=. tcp_source )  .||.
    (42 /=. tcp_dest   )  .||.
    (42 /=. tcp_hdrlen )  .||.
    (42 /=. udp_source )  .||.
    (42 /=. udp_dest   )  .||.
    (42 /=. udp_len    )  .||.
    (42 /=. icmp_type  )  .||.
    (42 /=. icmp_code  )

comparators11 =
    (42 >. ip_tos     )  .||.
    (42 >. ip_tot_len )  .||.
    (42 >. ip_id      )  .||.
    (42 >. ip_frag    )  .||.
    (42 >. ip_ttl     )  .||.
    (42 >. get_mark   )  .||.
    (42 >. get_state  )  .||.
    (42 >. tcp_source )  .||.
    (42 >. tcp_dest   )  .||.
    (42 >. tcp_hdrlen )  .||.
    (42 >. udp_source )  .||.
    (42 >. udp_dest   )  .||.
    (42 >. udp_len    )  .||.
    (42 >. icmp_type  )  .||.
    (42 >. icmp_code  )

comparators12 =
    (42 >=. ip_tos     )  .||.
    (42 >=. ip_tot_len )  .||.
    (42 >=. ip_id      )  .||.
    (42 >=. ip_frag    )  .||.
    (42 >=. ip_ttl     )  .||.
    (42 >=. get_mark   )  .||.
    (42 >=. get_state  )  .||.
    (42 >=. tcp_source )  .||.
    (42 >=. tcp_dest   )  .||.
    (42 >=. tcp_hdrlen )  .||.
    (42 >=. udp_source )  .||.
    (42 >=. udp_dest   )  .||.
    (42 >=. udp_len    )  .||.
    (42 >=. icmp_type  )  .||.
    (42 >=. icmp_code  )


cond = do
    conditional is_tcp (log_msg "tcp") unit
    when is_tcp (drop)
    unless is_ip kernel


filters = do
    ip
    udp
    tcp
    icmp
    vlan
    l3_proto 42
    l3_proto 0x842
    l4_proto 89
    flow
    rtp
    rtcp
    sip
    voip
    vlan_id_filter [1,2,3]
    no_frag
    no_more_frag
    port 12
    src_port 21
    dst_port 22
    addr "192.168.0.0/24"
    addr (CIDR ("192.168.0.0",24))
    src_addr "0.0.0.0/0"
    dst_addr "0.0.0.0/0"


steerings = do
    steer_rrobin
    steer_rss
    steer_to 1
    steer_link
    steer_local_link "4c:60:de:86:55:46"
    double_steer_mac
    steer_vlan
    steer_p2p
    double_steer_ip
    steer_local_ip "192.168.1.0/24"
    steer_flow
    steer_local_net "192.168.0.0" 16 24
    steer_field 14 2
    double_steer_field 14 18 2
    steer_field_symmetric 14 18 4
    steer_rtp
    steer_voip


forwarders = do
    kernel
    broadcast
    drop
    classify 11
    forward "eth1"
    forwardIO "eth2"
    link ["eth1", "eth2", "eth3"]
    bridge "eth2"
    tee "eth1" is_udp >-> kernel
    tap "eth1" is_udp >-> kernel


logging = do
    log_msg "ok"
    log_buff
    log_packet



blooming = do
    filter (bloom 1024 ["192.168.0.13", "192.168.0.42"] 32)
    filter (bloom_src 1024 ["192.168.0.13", "192.168.0.42"] 32)
    filter (bloom_dst 1024 ["192.168.0.13", "192.168.0.42"] 32)
    bloom_filter 1024 ["192.168.0.13", "192.168.0.42"] 32
    bloom_src_filter 1024 ["192.168.0.13", "192.168.0.42"] 32
    bloom_dst_filter 1024 ["192.168.0.13", "192.168.0.42"] 32


misc = do
    unit
    inc 10
    dec 11
    mark 42
    put_state 42


main = do
    filter predicates
    filter combinators
    filter comparators
    filter comparators2
    filter comparators3
    filter comparators4
    filter comparators5
    filter comparators6
    filter comparators7
    filter comparators8
    filter comparators9
    filter comparators10
    filter comparators11
    filter comparators12
    combinators2
    cond
    filters
    steerings
    forwarders
    logging
    blooming
    misc

