top_dir = ..
dev_dir = device
grub_dir = grub
lib_dir = lib
stage1_dir = stage1
stage2_dir = stage2
config_dir = config
stage1 = $(stage1_dir)/stage1
stage1_5 = $(stage2_dir)/e2fs_stage1_5
stage2 = $(stage2_dir)/stage2
config_file = $(config_dir)/menu.lst
default_file = $(config_dir)/default
#kernel = /boot/vmlinuz-2.6.32-754.el6.x86_64
#kernel = /home/kernel/linux-2.6.32/arch/x86/boot/bzImage
#kernel = /home/test/linux/arch/x86/boot/bzImage
#initrd = /boot/initramfs-2.6.32-754.el6.x86_64.img
embed_files = $(stage1) $(stage1_5) $(stage2) $(config_file) $(default_file)
os_image = $(dev_dir)/testOS.img
os_image_size=200
part_table = $(dev_dir)/sector1
boot_disk = $(dev_dir)/xxx
boot_mount = /mnt/xxx
boot_grub_dir = $(boot_mount)/$(grub_dir)
init_flag = .inited
boot_flag = .booted
embed_flag = .embedded

BOCHS = $(top_dir)/bochs/build/normal/bin/bochs
QEMU = $(top_dir)/qemu/qemu-1.4.0/x86_64-softmmu/qemu-system-x86_64
INSTALL_SCRIPT = _install

all: build

build:
	-cd $(lib_dir) && $(MAKE)
	-cd $(stage1_dir) && $(MAKE)
	-cd $(stage2_dir) && $(MAKE)
	-cd $(grub_dir) && $(MAKE)

run: install
	$(BOCHS) -q -f .bochsrc
	#$(QEMU) -drive file=$(os_image)

debug: install
	$(BOCHS) -q -f .bochsrc

debug_qemu:
	$(QEMU) -drive file=$(os_image) -s -S

$(init_flag):
	# create os_image 
	-dd if=/dev/zero of=$(os_image) bs=1024k seek=$(os_image_size) count=0
	-dd if=$(part_table) of=$(os_image) conv=notrunc
	-touch $@

$(boot_flag): $(kernel)
	# create boot disk
	-dd if=/dev/zero of=$(boot_disk) bs=1024k count=0 seek=100
	-mke2fs -t ext3 -F $(boot_disk)
	# mount boot disk
	-mkdir -p $(boot_mount)
	$(call mountBoot)
	-mkdir -p $(boot_grub_dir)
	-cp $(kernel) $(initrd) $(boot_mount)
	$(call umountBoot)
	-touch $@

$(embed_flag): $(init_flag) $(boot_flag) $(embed_files)
	# embed files
	$(call mountBoot)
	-cp $(embed_files) $(boot_grub_dir)
	$(call saveImage)
	# update stage2
	$(call mountBoot)
	-sh $(INSTALL_SCRIPT)
	$(call saveImage)
	-touch $@

install: $(embed_flag)
	:

.PHONY: clean_all clean_dev clean

clean_dev:
	-rm -f $(os_image) $(init_flag) $(embed_flag)

clean_boot:
	-rm -f $(boot_disk) $(boot_flag)

clean: clean_dev
	-cd $(lib_dir) && $(MAKE) clean
	-cd $(stage1_dir) && $(MAKE) clean
	-cd $(stage2_dir) && $(MAKE) clean
	-cd $(grub_dir) && $(MAKE) clean

clean_all: clean clean_boot



# saveImage()
define saveImage 
	# remount will flush all to boot disk
	$(call umountBoot)
	-dd if=$(boot_disk) of=$(os_image) conv=notrunc seek=2048
endef

# mountBoot()
define mountBoot
	-mount -o loop,sync $(boot_disk) $(boot_mount)
endef

# umountBoot()
define umountBoot
	-umount $(boot_disk)
endef
