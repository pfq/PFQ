/*******************************************************************************

  Intel 10 Gigabit PCI Express Linux driver
  Copyright(c) 1999 - 2010 Intel Corporation.

  This program is free software; you can redistribute it and/or modify it
  under the terms and conditions of the GNU General Public License,
  version 2, as published by the Free Software Foundation.

  This program is distributed in the hope it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
  more details.

  You should have received a copy of the GNU General Public License along with
  this program; if not, write to the Free Software Foundation, Inc.,
  51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.

  The full GNU General Public License is included in this distribution in
  the file called "COPYING".

  Contact Information:
  e1000-devel Mailing List <e1000-devel@lists.sourceforge.net>
  Intel Corporation, 5200 N.E. Elam Young Parkway, Hillsboro, OR 97124-6497

*******************************************************************************/

#include "ixgbe.h"

#ifdef IXGBE_FCOE
#include <linux/sysfs.h>
#include <linux/device.h>
#include <linux/netdevice.h>

/* Ethernet payload size for FCoE to be able to carry full sized FC Frames
 * 14 byte FCoE header + 24 byte FC header + 2112 max payload + 4 byte CRC
 * 	+ 4 byte FCoE trailing encapsulation = 2158
 * This is the Ethernet payload, replacing the default of 1500, and does
 * not include Ethernet headers, VLAN tags, or Ethernet CRC.
 */
#define IXGBE_FCOE_MTU	2158

static ssize_t ixgbe_show_fcoe_mtu(struct device *dev, struct device_attribute *attr, char *buf)
{
	return sprintf(buf, "%d\n", IXGBE_FCOE_MTU);
}

static struct device_attribute ixgbe_attrs[] = {
	__ATTR(fcoe-mtu, S_IRUGO, ixgbe_show_fcoe_mtu, NULL),
};

int ixgbe_sysfs_create(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	int err;
	int i;

	for (i = 0 ; i < ARRAY_SIZE(ixgbe_attrs); i++) {
		err = device_create_file(&netdev->dev, &ixgbe_attrs[i]);
		if (err)
			goto fail;
	}
	return 0;

fail:
	while (i-- >= 0)
		device_remove_file(&netdev->dev, &ixgbe_attrs[i]);
	return err;
}

void ixgbe_sysfs_remove(struct ixgbe_adapter *adapter)
{
	struct net_device *netdev = adapter->netdev;
	int i;

	for (i = 0 ; i < ARRAY_SIZE(ixgbe_attrs); i++)
		device_remove_file(&netdev->dev, &ixgbe_attrs[i]);
}
#endif /* IXGBE_FCOE */

