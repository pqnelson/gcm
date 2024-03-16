% "mptopdf img.mp" produces the results
outputtemplate := "arakawa-lamb-grid-%c.mps";
beginfig(0)
  numeric i;
  numeric u; u:=1pc;
  path p[];
  p[0] = (10u,0)--(-10u,0);
  p[3] = fullcircle scaled 6u;
  p[1] = fullcircle scaled 12u;
  p[2] = fullcircle scaled 18u;
  fill p3 withcolor 0.25[white,black];
  draw p[1];
  draw p2;
  draw p3;

  for i=0 upto 12:
    draw p0 rotated 30i;
  endfor;

  z[0] = 7.5u*(cosd(135),sind(135));
  z[1] = 7.5u*(cosd(120),sind(120));
  z[2] = 7.5u*(cosd(150),sind(150));
  z[3] = 6u*(cosd(135),sind(135));
  z[4] = 9u*(cosd(135),sind(135));

  for i=0 upto 4:
    label(btex $\bullet$ etex, z[i]);
  endfor;

  label.rt(btex $u_{i,k}$ etex, z1);
  label.bot(btex $u_{i-1,k}$ etex, z2 + (0,-3));
  label.lrt(btex $w_{i,k}$ etex, z3);
  label.ulft(btex $w_{i,k+1}$ etex, z4);
  label.top(btex $T_{i,k}$ etex, z0 + (0,1));
endfig;

outputtemplate := "boussinesq-grid-%c.mps";
beginfig(1)
  z0 = (-0.5u,0);
  z1 = (u,0);
  z2 = (4u,0);
  z3 = z2 + (0,1.25u);
  z4 = z1 + (0,1.25u);
  z5 = z0 + (0,1.25u);
  draw (-u,-0.5u)--(5.5u,-0.5u)--(5.5u,1.75u)--(-u,1.75u)--(-u,0.75u)--(3.25u,0.75u)--(3.25u,0.5u)--(-u,0.5u)--(-u,-0.5u);
  draw (3.25u,0.625u)--(5.5u,0.625u);
  draw (0.25u,-0.5u)--(0.25u,0.5u);
  draw (0.25u,0.75u)--(0.25u,1.75u);
  draw (1.75u,-0.5u)--(1.75u,0.5u);
  draw (1.75u,0.75u)--(1.75u,1.75u);
  % great
  draw (3u,-0.5u)--(3u,0.5u);
  draw (3u,0.75u)--(3u,1.75u);
  
  label(btex $\scriptstyle 1$ etex, z0);
  label(btex $\scriptstyle 2$ etex, z1);
  label(btex $\scriptstyle{1\over2}N$ etex, z2);
  label(btex $\scriptstyle{1\over2}N+1$ etex, z3);
  label(btex $\scriptstyle N-1$ etex, z4);
  label(btex $\scriptstyle N$ etex, z5);
  label(btex $\scriptstyle \dots$ etex, (0.5*(1.75u+3u),0));
  label(btex $\scriptstyle \dots$ etex, (0.5*(1.75u+3u),1.25u));
endfig;


beginfig(2)
  path p[];
  numeric R; R := 4u;
  numeric i;
  color lightgray; lightgray = (1/3)[white,black];
  for i=0 upto 7:
    z[i] = R*(cosd(45i-90),sind(45i-90));
    label(btex $\bullet$ etex, z[i]);
  endfor;
  for i=0 upto 4:
    p[i] = ((R + 0.5u)*(cosd(45i),sind(45i)))--((R + 0.5u)*(cosd(45i+180),sind(45i+180)));
  endfor;
  for i=0 upto 4:
    draw p[i] withcolor lightgray;
  endfor;
  draw fullcircle scaled 2R;
  label(btex $\bullet$ etex, (0,0));
  label.bot(btex $\lambda_{1}$ etex, z[0]+(0,-0.5u));
  label.lrt(btex $\lambda_{2}$ etex, z[1]+(0.5u,-0.5u));
  label.rt(btex $\dots$ etex, z[2]+(0.5u,0));
  label.urt(btex $\lambda_{(N/2)-1}$ etex, z[3]+(0.5u,0.5u));
  label.top(btex $\lambda_{(N/2)}$ etex, z[4]+(0,0.5u));
  label.ulft(btex $\dots$ etex, z[5]+(-0.5u,0.5u));
  label.llft(btex $\lambda_{N}$ etex, z[7]+(-0.5u,-0.5u));
  % drawarrow (z0 + (-2,2))--((0,-3) + (-2,-2));

  drawarrow z0 -- (0,-2);
  
endfig;

beginfig(3)
  path p[];
  numeric R; R := 4u;
  numeric i;
  color lightgray; lightgray = (1/3)[white,black];
  for i=0 upto 7:
    z[i] = R*(cosd(45i-90),sind(45i-90));
    label(btex $\bullet$ etex, z[i]);
  endfor;
  for i=0 upto 4:
    p[i] = ((R + 0.5u)*(cosd(45i),sind(45i)))--((R + 0.5u)*(cosd(45i+180),sind(45i+180)));
  endfor;
  for i=0 upto 4:
    draw p[i] withcolor lightgray;
  endfor;
  draw fullcircle scaled 2R;
  label(btex $\bullet$ etex, (0,0));
  label.bot(btex $\lambda_{1}$ etex, z[0]+(0,-0.5u));
  label.lrt(btex $\lambda_{2}$ etex, z[1]+(0.5u,-0.5u));
  label.rt(btex $\dots$ etex, z[2]+(0.5u,0));
  label.urt(btex $\lambda_{(N/2)-1}$ etex, z[3]+(0.5u,0.5u));
  label.top(btex $\lambda_{(N/2)}$ etex, z[4]+(0,0.5u));
  label.ulft(btex $\dots$ etex, z[5]+(-0.5u,0.5u));
  label.llft(btex $\lambda_{N}$ etex, z[7]+(-0.5u,-0.5u));

  drawarrow (0,0) -- (0.5*z[0] + (0,0)) withcolor red;
  for i=1 upto 7:
    drawarrow (0,0) -- (0.5*z[i]) withcolor red;
  endfor;
  
endfig;
end;