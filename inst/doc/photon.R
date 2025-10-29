## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------
library(photon)
options(photon_movers = FALSE)

## ----public_api, eval=FALSE---------------------------------------------------
# new_photon()
# #> <photon>
# #>   Type   : remote
# #>   Server : https://photon.komoot.io/

## ----geocode, eval=FALSE------------------------------------------------------
# cities1 <- geocode(c("Sanaa", "Caracas"), osm_tag = ":city")
# cities1
# #> Simple feature collection with 2 features and 12 fields
# #> Geometry type: POINT
# #> Dimension:     XY
# #> Bounding box:  xmin: -66.9146 ymin: 10.50609 xmax: 44.20588 ymax: 15.35386
# #> Geodetic CRS:  WGS 84
# #> # A tibble: 2 × 13
# #>     idx osm_type    osm_id osm_key osm_value type   countrycode name  country state county extent            geometry
# #>   <int> <chr>        <int> <chr>   <chr>     <chr>  <chr>       <chr> <chr>   <chr> <chr>  <list>         <POINT [°]>
# #> 1     1 N        258013552 place   city      distr… YE          Sana… Yemen   Aman… At Ta… <lgl>  (44.20588 15.35386)
# #> 2     2 R         11219583 place   city      city   VE          Cara… Venezu… Capi… Munic… <dbl>  (-66.9146 10.50609)

## ----reverse, eval=FALSE------------------------------------------------------
# cities2 <- reverse(cities1, osm_tag = ":city")
# cities2
# #> Simple feature collection with 2 features and 12 fields
# #> Geometry type: POINT
# #> Dimension:     XY
# #> Bounding box:  xmin: -66.9146 ymin: 10.50609 xmax: 44.20588 ymax: 15.35386
# #> Geodetic CRS:  WGS 84
# #> # A tibble: 2 × 13
# #>    idx osm_type    osm_id osm_key osm_value type   countrycode name  country state county extent            geometry
# #>  <int> <chr>        <int> <chr>   <chr>     <chr>  <chr>       <chr> <chr>   <chr> <chr>  <list>         <POINT [°]>
# #>1     1 N        258013552 place   city      distr… YE          Sana… Yemen   Aman… At Ta… <lgl>  (44.20588 15.35386)
# #>2     2 R         11219583 place   city      city   VE          Cara… Venezu… Capi… Munic… <dbl>  (-66.9146 10.50609)

## ----compare, eval=FALSE------------------------------------------------------
# all.equal(cities1, cities2)
# #> [1] TRUE

## ----java, eval=FALSE---------------------------------------------------------
# # pak::pkg_install("rJavaEnv")
# library(rJavaEnv)
# 
# # Consent to downloading Java
# rje_consent()
# #> Consent for using rJavaEnv has already been provided.
# 
# # Install and use Corretto JDK 24
# use_java(24)
# 
# # Check if installation was successful
# java_check_version_cmd()
# #> JAVA_HOME: /root/.cache/R/rJavaEnv/installed/linux/x64/24
# #> Java path: /root/.cache/R/rJavaEnv/installed/linux/x64/24/bin/java
# #> Java version: "openjdk version \"24.0.1\" 2025-04-15 OpenJDK Runtime Environment Corretto-24.0.1.9.1 (build
# #> 24.0.1+9-FR) OpenJDK 64-Bit Server VM Corretto-24.0.1.9.1 (build 24.0.1+9-FR, mixed mode, sharing)"
# #> [1] "24"

## ----local_photon, eval=FALSE-------------------------------------------------
# path <- file.path(tempdir(), "photon")
# photon <- new_photon(path, country = "Monaco")
# #> ℹ openjdk version "24.0.1" 2025-04-15
# #> ℹ OpenJDK Runtime Environment Corretto-24.0.1.9.1 (build 24.0.1+9-FR)
# #> ℹ OpenJDK 64-Bit Server VM Corretto-24.0.1.9.1 (build 24.0.1+9-FR, mixed mode, sharing)
# #> ✔ Successfully downloaded OpenSearch photon 0.7.0. [17.9s]
# #> ✔ Successfully downloaded search index. [854ms]
# #> • Version: 0.7.4
# #> • Coverage: Monaco
# #> • Time: 2025-09-17

## ----start, eval=FALSE--------------------------------------------------------
# photon$start()
# #> Cluster Name: photon
# #> Base Path:    /tmp/RtmpLQIqmy/photon/./photon_data
# #> Num Of Node:  1
# #> Node Name:      Node 1
# #> HTTP Port:      9201
# #> Data Directory: /tmp/RtmpLQIqmy/photon/./photon_data/node_1/data
# #> Log Directory:  /tmp/RtmpLQIqmy/photon/./photon_data/node_1/logs
# #> [2025-06-09T23:54:26,879][INFO ][o.o.n.Node               ] version[2.19.1], pid[1896], build[unknown/unknown/unknown], OS[Linux/6.6.87.1-microsoft-standard-WSL2/amd64], JVM[Amazon.com Inc./OpenJDK 64-Bit Server VM/24.0.1/24.0.1+9-FR]
# #> [2025-06-09T23:54:26,887][INFO ][o.o.n.Node               ] JVM home [/root/.cache/R/rJavaEnv/installed/linux/x64/24]
# #> [2025-06-09T23:54:26,899][INFO ][o.o.n.Node               ] JVM arguments []
# #> [2025-06-09T23:54:26,932][INFO ][o.o.p.PluginsService     ] no modules loaded
# #> [2025-06-09T23:54:26,934][INFO ][o.o.p.PluginsService     ] loaded plugin [org.opensearch.analysis.common.CommonAnalysisPlugin]
# #> [2025-06-09T23:54:26,934][INFO ][o.o.p.PluginsService     ] loaded plugin [org.opensearch.geo.GeoModulePlugin]
# #> [2025-06-09T23:54:26,934][INFO ][o.o.p.PluginsService     ] loaded plugin [org.opensearch.transport.Netty4Plugin]
# #> [2025-06-09T23:54:27,174][INFO ][o.a.l.s.MemorySegmentIndexInputProvider] Using MemorySegmentIndexInput and native madvise support with Java 21 or later; to disable start with -Dorg.apache.lucene.store.MMapDirectory.enableMemorySegments=false
# #> [2025-06-09T23:54:27,184][INFO ][o.o.e.NodeEnvironment    ] using [1] data paths, mounts [[/ (/dev/sdd)]], net usable_space [222.3gb], net total_space [250.9gb], types [ext4]
# #> [2025-06-09T23:54:27,190][INFO ][o.o.e.NodeEnvironment    ] heap size [2.4gb], compressed ordinary object pointers [true]
# #> [2025-06-09T23:54:27,384][WARN ][o.a.l.i.v.VectorizationProvider] You are running with Java 23 or later. To make full use of the Vector API, please update Apache Lucene.
# #> [2025-06-09T23:54:27,603][INFO ][o.o.n.Node               ] node name [Node 1], node ID [IFRXmNbwTYi4SLrEQXOn3A], cluster name [photon], roles [data, cluster_manager]
# #> [2025-06-09T23:54:27,708][INFO ][o.o.e.ExtensionsManager  ] ExtensionsManager initialized
# #> [2025-06-09T23:54:29,302][INFO ][o.o.t.NettyAllocator     ] creating NettyAllocator with the following configs: [name=opensearch_configured, chunk_size=512kb, suggested_max_allocation_size=512kb, factors={opensearch.unsafe.use_netty_default_chunk_and_page_size=false, g1gc_enabled=true, g1gc_region_size=2mb}]
# #> [2025-06-09T23:54:29,499][INFO ][o.o.d.DiscoveryModule    ] using discovery type [single-node] and seed hosts providers [settings]
# #> [2025-06-09T23:54:30,082][WARN ][o.o.g.DanglingIndicesState] gateway.auto_import_dangling_indices is disabled, dangling indices will not be automatically detected or imported and must be managed manually
# #> [2025-06-09T23:54:30,572][INFO ][o.o.n.Node               ] initialized
# #> [2025-06-09T23:54:30,572][INFO ][o.o.n.Node               ] starting ...
# #> [2025-06-09T23:54:30,723][INFO ][o.o.t.TransportService   ] publish_address {127.0.0.1:9300}, bound_addresses {[::1]:9300}, {127.0.0.1:9300}
# #> [2025-06-09T23:54:31,016][INFO ][o.o.c.c.Coordinator      ] cluster UUID [wbcIe8ZCQw-oX0uvwZgysw]
# #> [2025-06-09T23:54:31,199][INFO ][o.o.c.s.MasterService    ] Tasks batched with key: org.opensearch.cluster.coordination.JoinHelper, count:3 and sample tasks: elected-as-cluster-manager ([1] nodes joined)[{Node 1}{IFRXmNbwTYi4SLrEQXOn3A}{RKCnk_jPQpmMLN2B0oUoaA}{127.0.0.1}{127.0.0.1:9300}{dm}{shard_indexing_pressure_enabled=true} elect leader, _BECOME_CLUSTER_MANAGER_TASK_, _FINISH_ELECTION_], term: 2, version: 11, delta: cluster-manager node changed {previous [], current [{Node 1}{IFRXmNbwTYi4SLrEQXOn3A}{RKCnk_jPQpmMLN2B0oUoaA}{127.0.0.1}{127.0.0.1:9300}{dm}{shard_indexing_pressure_enabled=true}]}
# #> [2025-06-09T23:54:31,289][INFO ][o.o.c.s.ClusterApplierService] cluster-manager node changed {previous [], current [{Node 1}{IFRXmNbwTYi4SLrEQXOn3A}{RKCnk_jPQpmMLN2B0oUoaA}{127.0.0.1}{127.0.0.1:9300}{dm}{shard_indexing_pressure_enabled=true}]}, term: 2, version: 11, reason: Publication{term=2, version=11}
# #> [2025-06-09T23:54:31,316][INFO ][o.o.d.PeerFinder         ] setting findPeersInterval to [1s] as node commission status = [true] for local node [{Node 1}{IFRXmNbwTYi4SLrEQXOn3A}{RKCnk_jPQpmMLN2B0oUoaA}{127.0.0.1}{127.0.0.1:9300}{dm}{shard_indexing_pressure_enabled=true}]
# #> [2025-06-09T23:54:31,321][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:31,329][INFO ][o.o.h.AbstractHttpServerTransport] publish_address {127.0.0.1:9201}, bound_addresses {[::1]:9201}, {127.0.0.1:9201}
# #> [2025-06-09T23:54:31,330][INFO ][o.o.n.Node               ] started
# #> [2025-06-09T23:54:31,345][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:31,402][INFO ][o.o.g.GatewayService     ] recovered [1] indices into cluster_state
# #> [2025-06-09T23:54:31,403][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:31,599][INFO ][o.o.p.PluginsService     ] PluginService:onIndexModule index:[photon/3te8gZq4Q-u1LwWbZGtxPA]
# #> [2025-06-09T23:54:32,346][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:32,519][INFO ][o.o.c.r.a.AllocationService] Cluster health status changed from [RED] to [YELLOW] (reason: [shards started [[photon][2]]]).
# #> [2025-06-09T23:54:32,623][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:33,179][INFO ][d.k.p.App                ] Make sure that the ES cluster is ready, this might take some time.
# #> [2025-06-09T23:54:33,447][INFO ][d.k.p.App                ] ES cluster is now ready.
# #> [2025-06-09T23:54:33,637][INFO ][o.o.c.m.MetadataIndexStateService] closing indices [photon/3te8gZq4Q-u1LwWbZGtxPA]
# #> [2025-06-09T23:54:33,732][INFO ][o.o.c.m.MetadataIndexStateService] completed closing of indices [photon]
# #> [2025-06-09T23:54:33,733][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:33,837][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:33,936][INFO ][o.o.p.PluginsService     ] PluginService:onIndexModule index:[photon/3te8gZq4Q-u1LwWbZGtxPA]
# #> [2025-06-09T23:54:34,245][INFO ][o.o.c.m.MetadataIndexStateService] opening indices [[photon/3te8gZq4Q-u1LwWbZGtxPA]]
# #> [2025-06-09T23:54:34,252][INFO ][o.o.p.PluginsService     ] PluginService:onIndexModule index:[photon/3te8gZq4Q-u1LwWbZGtxPA]
# #> [2025-06-09T23:54:34,281][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:34,369][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:34,452][INFO ][o.o.p.PluginsService     ] PluginService:onIndexModule index:[photon/3te8gZq4Q-u1LwWbZGtxPA]
# #> [2025-06-09T23:54:34,741][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:34,880][INFO ][o.o.c.r.a.AllocationService] Cluster health status changed from [RED] to [YELLOW] (reason: [shards started [[photon][2]]]).
# #> [2025-06-09T23:54:34,970][WARN ][o.o.c.r.a.AllocationService] Falling back to single shard assignment since batch mode disable or multiple custom allocators set
# #> [2025-06-09T23:54:35,038][INFO ][o.o.p.PluginsService     ] PluginService:onIndexModule index:[photon/3te8gZq4Q-u1LwWbZGtxPA]
# #> [2025-06-09T23:54:35,165][INFO ][d.k.p.App                ] Starting API with the following settings:
# #>  Languages: [en, de, fr]
# #>  Import Date: 2025-06-01T03:00:01.000+0200
# #>  Support Structured Queries: false
# #>  Support Geometries: false
# #> ✔ Photon is now running. [12.8s]
# 
# photon$proc
# #> PROCESS 'java', running, pid 1896.

## ----is_ready, eval=FALSE-----------------------------------------------------
# photon$is_ready()
# #> [1] TRUE

## ----stop, eval=FALSE---------------------------------------------------------
# photon$stop()

## ----benchmark1, eval=FALSE---------------------------------------------------
# # offline geocoding
# bench::mark(with_photon(photon, geocode("Monte Carlo")), iterations = 25)$median
# #> [1] 17.1ms

## ----benchmark2, eval=FALSE---------------------------------------------------
# # online geocoding
# photon_pub <- new_photon(mount = FALSE)
# bench::mark(with_photon(photon_pub, geocode("Monte Carlo")), iterations = 25)$median
# #> [1] 1.04s

## ----purge, eval=FALSE--------------------------------------------------------
# photon$purge()
# #> ℹ Purging an instance kills the photon process and removes the photon directory.
# #> Continue? (y/N/Cancel) y

