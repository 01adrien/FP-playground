# syntax=docker/dockerfile:1
FROM racket/racket:8.18

RUN apt-get update \
    && apt-get install -y --no-install-recommends rlwrap make \
    && rm -rf /var/lib/apt/lists/*

RUN raco pkg install --auto --skip-installed --no-docs plait fmt racket-langserver

RUN cat <<'EOF' >> /root/.bashrc
alias ll='ls -alF'
alias racket='rlwrap racket'
PS1='\w\n\$ '
EOF

WORKDIR /workspaces
CMD ["bash"]