-- example
--

import Network.PFQ.Lang.Default

main = par (when (not is_tcp) drop) (filter (ip_ttl .>. 10))

