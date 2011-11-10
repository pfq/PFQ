#include <linux/module.h>
#include <linux/vermagic.h>
#include <linux/compiler.h>

MODULE_INFO(vermagic, VERMAGIC_STRING);

struct module __this_module
__attribute__((section(".gnu.linkonce.this_module"))) = {
 .name = KBUILD_MODNAME,
 .init = init_module,
#ifdef CONFIG_MODULE_UNLOAD
 .exit = cleanup_module,
#endif
 .arch = MODULE_ARCH_INIT,
};

static const struct modversion_info ____versions[]
__used
__attribute__((section("__versions"))) = {
	{ 0xae919a76, "module_layout" },
	{ 0xdfe1487f, "netdev_info" },
	{ 0xb2261e27, "pci_bus_read_config_byte" },
	{ 0x5a34a45c, "__kmalloc" },
	{ 0xf9a482f9, "msleep" },
	{ 0xc4dc87, "timecounter_init" },
	{ 0x4c4fef19, "kernel_stack" },
	{ 0x1766690c, "pci_enable_sriov" },
	{ 0xd6ee688f, "vmalloc" },
	{ 0x3ec8886f, "param_ops_int" },
	{ 0x91eb9b4, "round_jiffies" },
	{ 0xb63b5ccf, "skb_pad" },
	{ 0xd1b38764, "dev_set_drvdata" },
	{ 0xc4f331c6, "cpu_online_mask" },
	{ 0x79aa04a2, "get_random_bytes" },
	{ 0x1fac583b, "dma_set_mask" },
	{ 0xa1c67b66, "node_data" },
	{ 0x249da496, "napi_complete" },
	{ 0x37dd69df, "malloc_sizes" },
	{ 0xac478eb5, "pci_disable_device" },
	{ 0x99a8896, "pci_disable_msix" },
	{ 0x26733540, "netif_carrier_on" },
	{ 0xf4f44630, "pci_disable_sriov" },
	{ 0xb813ce5a, "timecompare_transform" },
	{ 0xf89843f9, "schedule_work" },
	{ 0xc0a3d105, "find_next_bit" },
	{ 0xc3964938, "netif_carrier_off" },
	{ 0xe9196ad5, "x86_dma_fallback_dev" },
	{ 0x637dd6, "driver_for_each_device" },
	{ 0xeae3dfd6, "__const_udelay" },
	{ 0x9e1bdc28, "init_timer_key" },
	{ 0x999e8297, "vfree" },
	{ 0x6633a80c, "pci_bus_write_config_word" },
	{ 0x2447533c, "ktime_get_real" },
	{ 0x47c7b0d2, "cpu_number" },
	{ 0x3c2c5af5, "sprintf" },
	{ 0xb2cc30b1, "__alloc_pages_nodemask" },
	{ 0xfdf29685, "netif_napi_del" },
	{ 0x7d11c268, "jiffies" },
	{ 0x2d2c1121, "__netdev_alloc_skb" },
	{ 0x27c33efe, "csum_ipv6_magic" },
	{ 0xb5d1b042, "__pskb_pull_tail" },
	{ 0x71de9b3f, "_copy_to_user" },
	{ 0xf5ca909f, "pci_set_master" },
	{ 0x1d0bfdf4, "dca3_get_tag" },
	{ 0xe1bc7ede, "del_timer_sync" },
	{ 0xde0bdcff, "memset" },
	{ 0x96e0b123, "pci_enable_pcie_error_reporting" },
	{ 0x2e471f01, "dca_register_notify" },
	{ 0x7a7bcfbb, "pci_enable_msix" },
	{ 0x9575365e, "pci_restore_state" },
	{ 0x8006c614, "dca_unregister_notify" },
	{ 0x1d15597e, "dev_err" },
	{ 0x27e1a049, "printk" },
	{ 0x9d607be6, "free_netdev" },
	{ 0xa1c76e0a, "_cond_resched" },
	{ 0x7ec9bfbc, "strncpy" },
	{ 0xe32de1eb, "register_netdev" },
	{ 0x16305289, "warn_slowpath_null" },
	{ 0x70f8635b, "__pci_enable_wake" },
	{ 0x286886e0, "dev_close" },
	{ 0xce095088, "mod_timer" },
	{ 0x8163ae11, "netif_napi_add" },
	{ 0xd6b8e852, "request_threaded_irq" },
	{ 0xb28d155e, "dca_add_requester" },
	{ 0x118ee127, "skb_pull" },
	{ 0xe6566ac0, "dev_kfree_skb_any" },
	{ 0xc717471d, "dev_open" },
	{ 0xe523ad75, "synchronize_irq" },
	{ 0x31cc7acb, "pci_find_capability" },
	{ 0x13a31d34, "pci_select_bars" },
	{ 0xc6cbbc89, "capable" },
	{ 0xc0bf6ead, "timecounter_cyc2time" },
	{ 0x7245b503, "netif_device_attach" },
	{ 0xaa90d550, "napi_gro_receive" },
	{ 0xd5d86dc0, "_dev_info" },
	{ 0x40a9b349, "vzalloc" },
	{ 0x78764f4e, "pv_irq_ops" },
	{ 0xcfed9a76, "kmem_cache_alloc_node_trace" },
	{ 0x618911fc, "numa_node" },
	{ 0x9e6aa210, "netif_device_detach" },
	{ 0xbfa52c76, "__alloc_skb" },
	{ 0x42c8de35, "ioremap_nocache" },
	{ 0x12a38747, "usleep_range" },
	{ 0x2b80505d, "pci_bus_read_config_word" },
	{ 0x9d8698c0, "__napi_schedule" },
	{ 0x9a4dbf6, "pci_cleanup_aer_uncorrect_error_status" },
	{ 0xf0fdf6cb, "__stack_chk_fail" },
	{ 0xa8dd4d2b, "kfree_skb" },
	{ 0x36875389, "__timecompare_update" },
	{ 0x2ca40b40, "eth_type_trans" },
	{ 0x7f631502, "pskb_expand_head" },
	{ 0x7ca88d34, "pci_unregister_driver" },
	{ 0xcc5005fe, "msleep_interruptible" },
	{ 0x6aa961ca, "kmem_cache_alloc_trace" },
	{ 0xd94743ef, "node_states" },
	{ 0xe52947e7, "__phys_addr" },
	{ 0xf6ebc03b, "net_ratelimit" },
	{ 0x5f940577, "pci_set_power_state" },
	{ 0xb994e8f0, "eth_validate_addr" },
	{ 0xe72c1a78, "pci_disable_pcie_error_reporting" },
	{ 0x37a0cba, "kfree" },
	{ 0x236c8c64, "memcpy" },
	{ 0x801678, "flush_scheduled_work" },
	{ 0xeeb89a21, "___pskb_trim" },
	{ 0xf59f197, "param_array_ops" },
	{ 0x1f1d822, "pci_disable_msi" },
	{ 0x5d8cddfd, "dma_supported" },
	{ 0xedc03953, "iounmap" },
	{ 0x4ed835ad, "pci_prepare_to_sleep" },
	{ 0x11ecfe40, "__pci_register_driver" },
	{ 0x2288378f, "system_state" },
	{ 0x60659adf, "put_page" },
	{ 0xb352177e, "find_first_bit" },
	{ 0x4cbbd171, "__bitmap_weight" },
	{ 0x9a811412, "dev_warn" },
	{ 0x6a5d174a, "unregister_netdev" },
	{ 0x9e0c711d, "vzalloc_node" },
	{ 0x9edbecae, "snprintf" },
	{ 0x82d8de7a, "pci_enable_msi_block" },
	{ 0x850d569a, "__netif_schedule" },
	{ 0xab9d1cb7, "consume_skb" },
	{ 0x577ce778, "dca_remove_requester" },
	{ 0x181e0d4c, "pci_enable_device_mem" },
	{ 0x2d59a573, "skb_tstamp_tx" },
	{ 0x148eb545, "skb_put" },
	{ 0xfc9098c, "pci_wake_from_d3" },
	{ 0x7bb3bafc, "pci_release_selected_regions" },
	{ 0xc670a343, "pci_request_selected_regions" },
	{ 0x77e2f33, "_copy_from_user" },
	{ 0xe0a7a4c2, "skb_copy_bits" },
	{ 0x2573122, "dev_get_drvdata" },
	{ 0x9e7d6bd0, "__udelay" },
	{ 0x2b03b4cd, "dma_ops" },
	{ 0xd214b1eb, "device_set_wakeup_enable" },
	{ 0xf20dabd8, "free_irq" },
	{ 0xc7d9942d, "pci_save_state" },
	{ 0x8316f3a0, "alloc_etherdev_mqs" },
};

static const char __module_depends[]
__used
__attribute__((section(".modinfo"))) =
"depends=dca";

MODULE_ALIAS("pci:v00008086d00001521sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001522sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001523sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001524sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d0000150Esv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d0000150Fsv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001527sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001510sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001511sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001516sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00000438sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d0000043Asv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d0000043Csv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00000440sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d000010C9sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d0000150Asv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001518sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d000010E6sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d000010E7sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d0000150Dsv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d00001526sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d000010E8sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d000010A7sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d000010A9sv*sd*bc*sc*i*");
MODULE_ALIAS("pci:v00008086d000010D6sv*sd*bc*sc*i*");

MODULE_INFO(srcversion, "CFB25B2A2C600443F3DF627");
