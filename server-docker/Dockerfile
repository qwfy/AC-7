FROM archlinux/base

ARG ARG_UID
ARG ARG_GID
RUN groupadd --gid $ARG_GID www
RUN useradd --uid $ARG_UID --gid $ARG_GID www

COPY mirrorlist /etc/pacman.d/mirrorlist
RUN pacman -Syu --noconfirm
RUN pacman -Syu --noconfirm apache nodejs npm neovim
RUN npm install --global --unsafe-perm --allow-root elm@0.19.0-bugfix6
COPY httpd.conf /etc/httpd/conf/httpd.conf
EXPOSE 8080
ENV HOME /srv/http/ui/elm-home
ENTRYPOINT ["/usr/bin/httpd", "-D", "FOREGROUND"]
