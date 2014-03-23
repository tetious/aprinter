#!/usr/bin/env bash
# 
# Simple build script crafted for the APrinter project to support multiple 
# architecture targets and build actions using an elegant commandline.
# 
# Copyright (c) 2014 Bernard `Guyzmo` Pratz
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
#########################################################################################
# SAM3X stuff

SAM3X_URL=(
    "http://www.atmel.com/images/asf-standalone-archive-3.14.0.86.zip"
)
SAM3X_CHECKSUM=(
    "9739afa2c8192bd181f2d4e50fa312dc4c943b7a6a093213e755c0c7de9c3ed3  asf-standalone-archive-3.14.0.86.zip"
)

configure_sam3x() {
    ASF_BASE_DIR=${DEPS}/asf-standalone-archive-3.14.0.86/
    ASF_DIR=${ASF_BASE_DIR}/xdk-asf-3.14.0
    
    CMSIS_DIR=${ASF_DIR}/sam/utils/cmsis/${ARCH}
    TEMPLATES_DIR=${CMSIS_DIR}/source/templates
    LINKER_SCRIPT=${ASF_DIR}/sam/utils/linker_scripts/${ARCH}/${ARCH}${SUBARCH}/gcc/flash.ld

    configure_arm

    FLAGS_CXX_LD+=(
        -Wno-deprecated-register
    )
    FLAGS_C_CXX+=(
        -D__${ARCH^^}${SUBARCH^^}E__ -DHEAP_SIZE=16384
        -DBOARD=${BOARD}
        -I"${CMSIS_DIR}/include"
        -I"${TEMPLATES_DIR}"
        -I"${ASF_DIR}/sam/utils"
        -I"${ASF_DIR}/sam/utils/preprocessor"
        -I"${ASF_DIR}/sam/utils/header_files"
        -I"${ASF_DIR}/sam/boards"
        -I"${ASF_DIR}/sam/drivers/pmc"
        -I"${ASF_DIR}/common/utils"
        -I"${ASF_DIR}/common/services/usb"
        -I"${ASF_DIR}/common/services/usb/udc"
        -I"${ASF_DIR}/common/services/clock"
        -I"${ASF_DIR}/common/services/sleepmgr"
        -I"${ASF_DIR}/common/services/usb/class/cdc"
        -I"${ASF_DIR}/common/services/usb/class/cdc/device"
        -I"${ASF_DIR}/common/boards"
        -I"${ASF_DIR}/thirdparty/CMSIS/Include"
        -I"${ASF_DIR}"
        -I aprinter/platform/at91${ARCH}
    )

    C_SOURCES+=(
        "${TEMPLATES_DIR}/exceptions.c"
        "${TEMPLATES_DIR}/system_${ARCH}.c"
        "${TEMPLATES_DIR}/gcc/startup_${ARCH}.c"
        "${ASF_DIR}/sam/drivers/pmc/pmc.c"
        
    )
    CXX_SOURCES+=(
        "aprinter/platform/at91${ARCH}/at91${ARCH}_support.cpp"
    )

    if [ "$ARCH" = "sam3u" ]; then
        FLAGS_C_CXX+=(
            -I"${ASF_DIR}/sam/drivers/pio"
            -I"${ASF_DIR}/common/services/ioport"
        )
    fi

    if [ $USE_USB_SERIAL -gt 0 ]; then
        cp "${ASF_DIR}/common/services/usb/class/cdc/device/udi_cdc.c" ${BUILD}/udi_cdc-hacked.c
        patch -p0 ${BUILD}/udi_cdc-hacked.c < ${ROOT}/patches/asf-cdc-tx.patch

        if [ "$ARCH" = "sam3x" ]; then
            FLAGS_C_CXX+=( -DUSB_SERIAL )
            C_SOURCES+=(
                "${ASF_DIR}/sam/drivers/uotghs/uotghs_device.c"
            )
        elif [ "$ARCH" = "sam3u" ]; then
            C_SOURCES+=(
                "${ASF_DIR}/sam/drivers/udphs/udphs_device.c"
            )
        fi

        C_SOURCES+=(
            "${ASF_DIR}/sam/drivers/pmc/sleep.c"
            "${ASF_DIR}/common/utils/interrupt/interrupt_sam_nvic.c"
            "${ASF_DIR}/common/services/usb/udc/udc.c"
            ${BUILD}/udi_cdc-hacked.c
            "${ASF_DIR}/common/services/usb/class/cdc/device/udi_cdc_desc.c"
            "${ASF_DIR}/common/services/clock/${ARCH}/sysclk.c"
        )
    fi

    # define target functions
    INSTALL=install_sam3x
    RUNBUILD=build_arm
    UPLOAD=upload_sam3x
    FLUSH=flush_sam3x
    CHECK=check_depends_sam3x
}

check_depends_sam3x() {
    check_depends_arm
    [ -d ${ASF_DIR} ] || fail "Atmel Software Framework missing in dependences"
    [ -f ${DEPS}/bossa-code/bin/bossac ] || fail "Missing Sam3x upload tool 'bossac'"
}

upload_sam3x() {
    echo "  Uploading to Sam3X MCU"
    ${DEPS}/bossa-code/bin/bossac -p $1 -U false -i -e -w -v -b ${TARGET}.bin -R 
}

flush_sam3x() {
    flush_arm
    echo "  Flushing SAM3X toolchain"
    rm -rf ${ASF_BASE_DIR}
    rm -rf ${DEPS}/bossa-code
}

install_sam3x() {
    install_arm

    [ -d ${ASF_DIR} ] && \
    [ -f ${DEPS}/bossa-code/bin/bossac ] && \
    echo "   [!] SAM3X toolchain already installed" && return 0

    retr_and_extract SAM3X_URL[@] SAM3X_CHECKSUM[@]

    # install SAM3X flasher
    (
    cd ${DEPS}
    git clone git://git.code.sf.net/p/b-o-s-s-a/code bossa-code
    cd bossa-code
    make strip-bossac
    )
    echo "  Installation of Atmel Software Framework and bossac for SAM3X: success"
}

