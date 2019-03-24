FROM archlinux/base

ARG ARG_UID
ARG ARG_GID
RUN groupadd --gid $ARG_GID www
RUN useradd --uid $ARG_UID --gid $ARG_GID www

COPY mirrorlist /etc/pacman.d/mirrorlist
RUN pacman -Syu --noconfirm apache neovim
COPY httpd.conf /etc/httpd/conf/httpd.conf
EXPOSE 8080
ENTRYPOINT ["/usr/bin/httpd", "-D", "FOREGROUND"]
