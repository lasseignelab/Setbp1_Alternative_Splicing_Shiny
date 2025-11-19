IMAGE="acrumley/setbp1-shiny:0.1.1"
SIF="setbp1-shiny_0.1.1.sif"

if [[ ! -f "$SIF" ]]; then
    echo "$SIF not found — pulling image..."
    singularity pull docker://$IMAGE
else
    echo "$SIF already present."
fi

singularity exec \
    --cleanenv \
    --containall \
    --bind run:/run,var-lib-rstudio-server:/var/lib/rstudio-server,database.conf:/etc/rstudio/database.conf,rstudio_tmp:/tmp \
    --bind ../..:/project \
    --bind $USER_DATA/${USER}_secure_cookie \
    $SIF \
rserver \
    --auth-validate-users=0 \
    --auth-none=0  \
    --auth-pam-helper-path=pam-helper \
    --www-address=127.0.0.1 \
        --secure-cookie-key-file $USER_DATA/${USER}_secure_cookie \
        --server-user=$USER \
        --www-port 8787

