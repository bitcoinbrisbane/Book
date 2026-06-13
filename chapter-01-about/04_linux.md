# Linux

Yes you read correctly. If we're going to begin our cyberpunk journey, you'll need your favourite hoodie paired with Linux. In all seriousness, when I started developing for bitcoin projects, most of the documentation was Linux/bash based. Also, the bitcoin core compiler was easier to run in Linux and the community didn't give much support for Windows. Instead of trying to fight it, I installed Linux instead.

I was first exposed to Linux in the late 90s. I remember a course on basic Unix/Linux commands. The good news is that those commands haven't changed in the last 30 years, and the GUIs are better than Windows now anyway. All my favourite developer tools, such as VS Code, Postman and collaboration tools like Slack and Telegram are all Linux compatible, so there's really no excuse not to use a Linux distro as your daily desktop driver. Cloud based services like Office 365 and Gmail are generally good enough for day to day work or developer requirements. That said, the only application I sometimes miss is Adobe Photoshop, but I do have a Mac for that.

## Distros

So where do we start? Linux "flavours" are called Distros, short for distributions. Distros are various versions of the Linux operating system, each with its own unique features, package management systems, and intended use cases. They are built on the Linux kernel and typically include a collection of software applications, system libraries, and tools.

I'm going to recommend you use Ubuntu. You can download the .iso file from Ubuntu for free of course. If you're already using Windows, dual booting is a great option when starting out. There are great resources on the internet and it might depend on your system as to the exact instructions, but overall the steps should be something like:

1. Create a bootable USB from Windows (or Mac or Linux). Windows users can use a tool called Etcher.
2. Reboot your system and edit your BIOS to boot off the USB.
3. Create a new partition to install Linux on.
4. Linux will add a boot menu called GRUB. You can modify this easily to select the default boot options.

> **Note:** See https://opensource.com/article/18/5/dual-boot-linux

## My Top 4 Distros

### 1. Ubuntu

- **Base:** Debian
- **Package Manager:** APT (Advanced Package Tool)
- **Desktop Environments:** GNOME (default), KDE Plasma, XFCE, others
- **Target Users:** General users, beginners, and professionals
- **Features:** Ubuntu is known for its user-friendliness, extensive documentation, and large community support. It is often recommended for Linux newcomers and is widely used in both personal and professional environments.

### 2. Debian

- **Base:** Independent (one of the oldest and foundational distros)
- **Package Manager:** APT
- **Desktop Environments:** GNOME, KDE Plasma, XFCE, LXDE, others
- **Target Users:** Intermediate to advanced users, server administrators
- **Features:** Debian is renowned for its stability and reliability. It serves as the foundation for many other distributions, including Ubuntu. Debian prioritises free software and has a rigorous testing process for its packages.

### 3. Fedora

- **Base:** Independent (sponsored by Red Hat)
- **Package Manager:** DNF (Dandified YUM)
- **Desktop Environments:** GNOME (default), KDE Plasma, XFCE, others
- **Target Users:** Developers, advanced users, and those looking for cutting-edge software
- **Features:** Fedora is known for its focus on innovation, integrating the latest technologies, and serving as a testing ground for Red Hat Enterprise Linux (RHEL). It has a rapid release cycle and is popular among developers.

### 4. Linux Mint

- **Base:** Ubuntu (with a Debian-based version called LMDE)
- **Package Manager:** APT
- **Desktop Environments:** Cinnamon, MATE, XFCE
- **Target Users:** Beginners, general users
- **Features:** Linux Mint focuses on providing a user-friendly and comfortable desktop experience. It includes multimedia codecs out-of-the-box and offers a familiar desktop interface, making it popular among new Linux users.

> I use Arch BTW.
