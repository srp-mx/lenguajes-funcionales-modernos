#!/usr/bin/python3
from sys import platform
from enum import Enum
import subprocess
import configparser
import requests
import os

class Distro(Enum):
    DESCONOCIDO = 0
    ARCH = 1
    DEBIAN = 2
    FEDORA = 3
    UBUNTU = 4
    MINT = 5

def linux_distro():
    proc = subprocess.run(["cat", "/etc/os-release"], capture_output=True)
    if proc.returncode != 0:
        return Distro.DESCONOCIDO
    configStr = "[X]\n" + proc.stdout.decode("utf-8")
    config = configparser.ConfigParser()
    config.read_string(configStr)
    match config['X']['ID']:
        case 'arch':
            return Distro.ARCH
        case 'fedora':
            return Distro.FEDORA
        case 'ubuntu':
            return Distro.UBUNTU
        case 'debian':
            return Distro.DEBIAN
        case 'linuxmint':
            return Distro.MINT
        case _:
            return Distro.DESCONOCIDO

###### MAIN #######

print("Las siguientes instalaciones pueden tomar un tiempo")
if platform == "linux" or platform == "linux2": # Linux
    distro = linux_distro()
    if distro == Distro.DESCONOCIDO:
        print("No soportamos tu distro")
        exit(2)
    match distro:
        case Distro.ARCH:
            subprocess.run(["sudo", "pacman", "-S",
                            "texlive-basic",
                            "texlive-bibtexextra",
                            "texlive-bin",
                            "texlive-binextra",
                            "texlive-fontsextra",
                            "texlive-fontsrecommended",
                            "texlive-fontutils",
                            "texlive-formatsextra",
                            "texlive-games",
                            "texlive-humanities",
                            "texlive-langenglish",
                            "texlive-langspanish",
                            "texlive-latex",
                            "texlive-latexextra",
                            "texlive-latexrecommended",
                            "texlive-luatex",
                            "texlive-mathscience",
                            "texlive-pictures",
                            "texlive-plaingeneric",
                            "texlive-publishers",
                            "biber"])
        case Distro.DEBIAN | Distro.UBUNTU | Distro.MINT:
            subprocess.run(["sudo", "apt-get", "install", "texlive-full"])
        case Distro.FEDORA:
            subprocess.run(["sudo", "dnf", "install", "texlive-scheme-full"])
elif platform == "darwin": # Mac
    brew = subprocess.run(["which", "brew"], capture_output=True)
    if brew.returncode != 0:
        print("Instala Homebrew y vuelve a intentar.")
        exit(2)
    subprocess.run(["brew", "install", "grep"]) # instala ggrep (gnu grep)
    subprocess.run(["brew", "install", "--cask", "mactex-no-gui"])
elif platform == "win32": # Windows
    downl = "https://mirror.ctan.org/systems/texlive/tlnet/install-tl-windows.exe"
    downl_name = os.path.basename(downl)
    print(f"Descargando {downl_name} desde {downl} ...")
    response = requests.get(downl, stream=True)
    response.raise_for_status()  # Stop if there was an HTTP error
    with open(downl_name, "wb") as f:
        for chunk in response.iter_content(chunk_size=8192):
            if chunk:
                f.write(chunk)
    print(f"Descarga completa: {downl_name}")
    print("Selecciona instalar TeX Live Full.")
    subprocess.run([downl_name], check=True)
else:
    print("No soportamos", platform)
    exit(1)
