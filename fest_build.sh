#!/bin/sh
###########################################################################
##                                                                       ##
##                     Carnegie Mellon University                        ##
##                         Copyright (c) 2017                            ##
##                        All Rights Reserved.                           ##
##                                                                       ##
##  Permission is hereby granted, free of charge, to use and distribute  ##
##  this software and its documentation without restriction, including   ##
##  without limitation the rights to use, copy, modify, merge, publish,  ##
##  distribute, sublicense, and/or sell copies of this work, and to      ##
##  permit persons to whom this work is furnished to do so, subject to   ##
##  the following conditions:                                            ##
##   1. The code must retain the above copyright notice, this list of    ##
##      conditions and the following disclaimer.                         ##
##   2. Any modifications must be clearly marked as such.                ##
##   3. Original authors' names are not deleted.                         ##
##   4. The authors' names are not used to endorse or promote products   ##
##      derived from this software without specific prior written        ##
##      permission.                                                      ##
##                                                                       ##
##  CARNEGIE MELLON UNIVERSITY AND THE CONTRIBUTORS TO THIS WORK         ##
##  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ##
##  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ##
##  SHALL CARNEGIE MELLON UNIVERSITY NOR THE CONTRIBUTORS BE LIABLE      ##
##  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ##
##  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ##
##  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ##
##  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ##
##  THIS SOFTWARE.                                                       ##
##                                                                       ##
###########################################################################
##                                                                       ##
##  Example script used to test the Festival 2.5/Flite 2.1 release       ##
##                                                                       ##
##  Downloads code, compiles it, runs the voices, and builds a voice     ##
##                                                                       ##
###########################################################################

## Download the packed pieces of code/data if not already there
mkdir -p packed
if [ ! -f packed/SPTK-3.6.tar.gz ]
then
   (cd packed && wget http://festvox.org/packed/SPTK-3.6.tar.gz)
fi

if [ ! -f packed/festvox_kallpc16k.tar.gz ]
then
    (cd packed && wget http://www.festvox.org/packed/festival/2.4/voices/festvox_kallpc16k.tar.gz)
fi

if [ ! -f packed/festlex_CMU.tar.gz ]
then
    (cd packed && wget http://www.festvox.org/packed/festival/2.4/festlex_CMU.tar.gz)
fi

if [ ! -f packed/festlex_POSLEX.tar.gz ]
then
    (cd packed && wget http://www.festvox.org/packed/festival/2.4/festlex_POSLEX.tar.gz)
fi

if [ ! -f packed/awb100.tar.bz2 ]
then
    (cd packed && wget http://www.festvox.org/packed/data/cmu/awb100.tar.bz2)
fi

if [ ! -f packed/rms100.tar.bz2 ]
then
    (cd packed && wget http://www.festvox.org/packed/data/cmu/rms100.tar.bz2)
fi

### clone/unpack the code base
mkdir build
cd build

tar zxvf ../packed/SPTK-3.6.tar.gz
if [ -f ../packed/speech_tools-2.5.1-current.tar.gz ]
then
    tar zxvf ../packed/speech_tools-2.5.1-current.tar.gz
else
    git clone https://github.com/festvox/speech_tools
fi

if [ -f ../packed/festival-2.5.1-current.tar.gz ]
then
    tar zxvf ../packed/festival-2.5.1-current.tar.gz
else
    git clone https://github.com/festvox/festival
fi

if [ -f ../packed/festvox-2.8.2-current.tar.gz ]
then
    tar zxvf ../packed/festvox-2.8.2-current.tar.gz
else    
    git clone https://github.com/festvox/festvox
fi

if [ -f ../packed/flite-2.2-current.tar.bz2 ]
then
    tar jxvf ../packed/flite-2.2-current.tar.bz2
    mv flite-2.2-current flite
else
    git clone https://github.com/festvox/flite
fi

### Compile the code

export ESTDIR=`pwd`/speech_tools
export FLITEDIR=`pwd`/flite
export FESTVOXDIR=`pwd`/festvox
export SPTKDIR=`pwd`/SPTK
mkdir SPTK

patch -p0 <festvox/src/clustergen/SPTK-3.6.patch 
cd SPTK-3.6
./configure --prefix=$SPTKDIR
make
make install
cd ..

cd speech_tools
./configure
make
make test
cd ..

cd festival
./configure
make
cd ..

tar zxvf ../packed/festlex_CMU.tar.gz
tar zxvf ../packed/festlex_POSLEX.tar.gz
tar zxvf ../packed/festvox_kallpc16k.tar.gz

cd festvox
./configure
make
cd ..

cd flite
./configure
make
cd ..

echo "add to your shell startup script (\$HOME/.bashrc)"
echo export ESTDIR=$ESTDIR  >festvox_env_settings
echo export FLITEDIR=$FLITEDIR >>festvox_env_settings
echo export FESTVOXDIR=$FESTVOXDIR >>festvox_env_settings
echo export SPTKDIR=$SPTKDIR >>festvox_env_settings











