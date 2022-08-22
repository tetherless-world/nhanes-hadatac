FROM jupyter/r-notebook

LABEL maintainer="Henrique Santos <oliveh@rpi.edu>"

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

USER root
USER ${NB_UID}

RUN mamba install --quiet --yes \
    'r-survey' && \
    mamba clean --all -f -y && \
    fix-permissions "${CONDA_DIR}" && \
    fix-permissions "/home/${NB_USER}"

WORKDIR /home/${NB_USER}/

COPY --chown=${NB_USER}:users set_server_password.py .
RUN python set_server_password.py

ADD --chown=${NB_USER}:users notebooks /home/${NB_USER}/

RUN pip install voila
RUN jupyter serverextension enable voila
RUN jupyter server extension enable voila