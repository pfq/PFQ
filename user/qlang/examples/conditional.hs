-- example
--

import Network.PFQ.Lang.Prelude

main = par (when (not is_tcp) drop) (filter (ip_ttl .>. 10))

