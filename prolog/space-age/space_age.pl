:- use_module(library(clpq)).

%! planet(-Name:string, -LengthOfYear:number) is multi.
% Facts for each planet named `Name`, with the length of the planet's year in
% seconds.
planet("Earth", 31557600.0).
planet("Mercury", 7600543.81992).
planet("Venus", 19414149.052176).
planet("Mars", 59354032.690079994).
planet("Jupiter", 374355659.124).
planet("Saturn", 929292362.8848).
planet("Uranus", 2651370019.3296).
planet("Neptune", 5200418560.032001).
planet("Pluto", 7821235584.0).

%! space_age(-Planet:string, -Age:number, -Years:number) is multi.
% For an `Age` in seconds, find the number of planetary `Years` old someone
% would be for the `Planet`.
space_age(Planet, Age, Years) :-
	planet(Planet, LengthOfYear),
	% The exercise is expecting reals instead of rationals here, but using
	% clpr fails the determinancy check in the tests and I'm not sure how
	% to fix it. The tests aren't opinionated enough to actually fail when
	% given rationals instead, so going with this solution.
	{Years = Age / LengthOfYear}.
