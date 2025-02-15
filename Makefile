top_dir = ..
dev_dir = device
grub_dir = grub
lib_dir = lib
stage1_dir = stage1
stage2_dir = stage2
config_dir = config
stage1 = $(stage1_dir)/stage1
stage1_exec = $(stage1).exec
stage1_5 = $(stage2_dir)/e2fs_stage1_5
stage2 = $(stage2_dir)/stage2
config_file = $(config_dir)/menu.lst
default_file = $(config_dir)/default
linux_home = /home/kernel/my-linux
kernel = $(linux_home)/arch/x86/boot/bzImage
vmlinux = $(linux_home)/vmlinux
initrd = $(linux_home)/initramfs-2.6.32.27_xxx.img
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

INSTALL_SCRIPT = _install

BOCHS = $(top_dir)/bochs/build/normal/bin/bochs

QEMU = $(top_dir)/qemu/qemu-1.4.0/x86_64-softmmu/qemu-system-x86_64
QEMU_OPTS = -m 6144 -drive file=$(os_image) -cpu Haswell \
			-smp sockets=1,cores=2,threads=2 \
			-numa node,cpus=0-1,nodeid=0 \
			-numa node,cpus=2-3,nodeid=1 \
			-smbios type=1 \
			-rtc base=localtime -global pit.model=accurate -icount shift=auto


#			-smp sockets=1,cores=1,threads=1 \
#			-numa node,cpus=0,nodeid=0


all: build

build:
	-cd $(lib_dir) && $(MAKE)
	-cd $(stage1_dir) && $(MAKE)
	-cd $(stage2_dir) && $(MAKE)
	-cd $(grub_dir) && $(MAKE)

bochs: install
	$(BOCHS) -q -f .bochsrc

qemu: install
	$(QEMU) $(QEMU_OPTS)

debug_qemu: install
	$(QEMU) $(QEMU_OPTS) -s -S 

gdb_linux:
	gdb -ex "file $(vmlinux)" \
		-ex 'set arch i386:x86-64:intel' \
		-ex 'target remote localhost:1234' \
		-ex 'break x86_64_start_kernel' \
		-ex 'continue' \
		-ex 'disconnect' \
		-ex 'set arch i386:x86-64' \
		-ex 'target remote localhost:1234' \
		-ex 'set print array on' \
		-ex 'set print pretty on'

gdb_stage1:
	gdb -ex "file $(stage1_exec)" \
		-ex 'target remote localhost:1234' \
		-ex 'break *0x7c00' \
		-ex 'continue' \


$(init_flag):
	# create os_image 
	-dd if=/dev/zero of=$(os_image) bs=1024k seek=$(os_image_size) count=0
	-dd if=$(part_table) of=$(os_image) conv=notrunc
	-touch $@

$(boot_flag): $(kernel) $(initrd)
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

.PHONY: clean_all clean_dev clean gdb_qemu gdb_grub


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
