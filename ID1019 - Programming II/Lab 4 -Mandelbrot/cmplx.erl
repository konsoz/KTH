%% @author konstantin
%% @doc @todo Add description to cmplx.


-module(cmplx).
-compile(export_all).



new(Real,Img) -> {Real,Img}.

add({Real1,Img1},{Real2,Img2}) -> {Real1+Real2,Img1+Img2}.

sqr({Real,Img}) -> {Real*Real-Img*Img,Real*Img+Real*Img}.  

abs({Real,Img}) -> math:sqrt(Real*Real + Img*Img).