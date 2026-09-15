!> @file field_metadata.f90
!> @brief CF attribute catalog for output field variables.
!>
!> Hand-written; the registry `variables:` block describes the same
!> attributes and `tools/gen_registry.py --check` fails when they differ.
module model_field_metadata_mod
   use core_constants_mod, only: SP
   use core_output_channel_mod, only: type_var_meta
   implicit none
   public

contains

   !> To look up CF variable attributes by registry field name; an
   !> uncataloged name returns blank meta, which the writer renders
   !> as no attrs.
   pure function field_meta(name) result(m)
      character(*), intent(in) :: name
      type(type_var_meta) :: m

      select case (trim(name))
      case ("eta")
         m%units = "m"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "free surface elevation"
         m%standard_name = "sea_surface_height_above_mean_sea_level"
      case ("u")
         m%units = "m s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "depth-averaged x-velocity"
         m%standard_name = "eastward_sea_water_velocity"
      case ("v")
         m%units = "m s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "depth-averaged y-velocity"
         m%standard_name = "northward_sea_water_velocity"
      case ("p_flux")
         m%units = "m2 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "x-component of depth-integrated volume flux"
         m%funwave_name = "sea_water_x_volume_flux"
      case ("q_flux")
         m%units = "m2 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "y-component of depth-integrated volume flux"
         m%funwave_name = "sea_water_y_volume_flux"
      case ("depth")
         m%units = "m"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "still-water depth"
         m%standard_name = "sea_floor_depth_below_mean_sea_level"
      case ("h")
         m%units = "m"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "total water depth"
         m%standard_name = "sea_floor_depth_below_sea_surface"
      case ("mask")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "wet/dry mask"
         m%funwave_name = "sea_water_wet_binary_mask"
         m%n_flags = 2
         m%flag_values(1:2) = [0.0_SP, 1.0_SP]
         m%flag_meanings = "dry wet"
      case ("mask9")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "wet/dry mask on the 3x3 stencil"
         m%funwave_name = "sea_water_3x3_wet_binary_mask"
         m%n_flags = 2
         m%flag_values(1:2) = [0.0_SP, 1.0_SP]
         m%flag_meanings = "dry wet"
      case ("p")
         m%units = "m2 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "x depth-integrated volume flux"
         m%funwave_name = "eastward_depth_integrated_volume_flux_per_unit_width"
      case ("q")
         m%units = "m2 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "y depth-integrated volume flux"
         m%funwave_name = "northward_depth_integrated_volume_flux_per_unit_width"
      case ("velocity.mag")
         m%units = "m s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "depth-averaged sea water speed"
         m%standard_name = "sea_water_speed"
      case ("velocity.dir")
         m%units = "degree"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "current direction, CCW from +x axis"
         m%funwave_name = "depth_averaged_current_direction_from_x_axis"
      case ("vorticity")
         m%units = "s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "relative vorticity of the depth-averaged flow"
         m%standard_name = "ocean_relative_vorticity"
      case ("xi_0")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "offshore surf similarity parameter"
         m%funwave_name = "surf_similarity_parameter_offshore"
         m%comment = "bed slope / sqrt(H0 / L0); H0, T from the wavemakers (2a and T, or Hm0 and T_m-1,0), L0 = g T^2 / 2 pi"
      case ("xi_b")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "breaker surf similarity parameter"
         m%funwave_name = "surf_similarity_parameter_at_breaking"
         m%comment = "slope / sqrt(H_b / L0) at the last onset, H_b = envelope height; fill before the first (always under shock)"
      case ("gamma_b")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "breaker index"
         m%funwave_name = "breaker_height_to_depth_ratio"
         m%comment = "H_b over (still depth + envelope mid-level) at the last breaking onset; fill until the first onset"
      case ("front_steepness")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "peak surface rise rate over the shallow-water celerity"
         m%funwave_name = "peak_breaking_front_rise_rate_over_shallow_water_celerity"
         m%comment = "max of eta_t / sqrt(g h) over the last breaking event (breaking.cbrk1/cbrk2 gate this); fill until the first"
      case ("a")
         m%units = "m s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "divergence of the depth-integrated velocity"
         m%funwave_name = "divergence_of_depth_integrated_velocity"
         m%comment = "A = div(h u), h the still-water depth; w(z) = -A - z B to leading order"
      case ("b")
         m%units = "s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "divergence of the depth-averaged velocity"
         m%funwave_name = "divergence_of_depth_averaged_velocity"
         m%comment = "B = div u; see a for the vertical velocity"
      case ("a_x")
         m%units = "s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "x-gradient of the depth-integrated velocity divergence"
         m%funwave_name = "x_derivative_of_divergence_of_depth_integrated_velocity"
         m%comment = "A = div(h u); u(z) = u + (z_a - z) a_x + (z_a^2 - z^2)/2 b_x, z_a = (1 + beta_ref) eta + beta_ref h"
      case ("a_y")
         m%units = "s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "y-gradient of the depth-integrated velocity divergence"
         m%funwave_name = "y_derivative_of_divergence_of_depth_integrated_velocity"
         m%comment = "A = div(h u); v(z) = v + (z_a - z) a_y + (z_a^2 - z^2)/2 b_y, z_a = (1 + beta_ref) eta + beta_ref h"
      case ("b_x")
         m%units = "m-1 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "x-gradient of the velocity divergence"
         m%funwave_name = "x_derivative_of_divergence_of_depth_averaged_velocity"
         m%comment = "B = div u; see a_x for the profile"
      case ("b_y")
         m%units = "m-1 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "y-gradient of the velocity divergence"
         m%funwave_name = "y_derivative_of_divergence_of_depth_averaged_velocity"
         m%comment = "B = div u; see a_y for the profile"
      case ("momentum_flux")
         m%units = "m3 s-2"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "depth-integrated momentum flux magnitude"
         m%funwave_name = "depth_integrated_momentum_flux_magnitude"
      case ("nu_break")
         m%units = "m2 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "breaking eddy viscosity"
         m%funwave_name = "breaking_eddy_viscosity"
      case ("age_break")
         m%units = "s"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "age of the local breaking event"
         m%funwave_name = "breaking_event_age"
      case ("roller_flux")
         m%units = "m2 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "surface roller volume flux per unit width"
         m%funwave_name = "surface_roller_volume_flux_per_unit_width"
      case ("undertow_u")
         m%units = "m2 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "x-component of the roller-driven return flux per unit width"
         m%funwave_name = "eastward_roller_return_volume_flux_per_unit_width"
      case ("undertow_v")
         m%units = "m2 s-1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "y-component of the roller-driven return flux per unit width"
         m%funwave_name = "northward_roller_return_volume_flux_per_unit_width"
      case ("breaking_active")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "breaker viscosity active flag"
         m%funwave_name = "breaking_eddy_viscosity_active_flag"
         m%n_flags = 2
         m%flag_values(1:2) = [0.0_SP, 1.0_SP]
         m%flag_meanings = "inactive active"
         m%comment = "nu_break above breaking.nu_bkg at the last stage, the wavemaker-zone term included"
      case ("nu_capped")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "breaker viscosity cap engaged flag"
         m%funwave_name = "breaking_eddy_viscosity_cap_engaged_flag"
         m%n_flags = 2
         m%flag_values(1:2) = [0.0_SP, 1.0_SP]
         m%flag_meanings = "uncapped capped"
         m%comment = "nu_break sitting at the breaking.nu_cap explicit-diffusion clamp at the last stage"
      case ("froude_scale")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "Froude cap velocity scale factor"
         m%funwave_name = "froude_cap_velocity_scale_factor"
         m%comment = "factor applied to the velocity by numerics.froude_cap at the last stage; 1 = untouched"
      case ("disp_gate")
         m%units = "1"
         m%has_fill = .true.
         m%fill_value = -9999.0_SP
         m%long_name = "dispersion gate weight"
         m%funwave_name = "dispersion_gate_weight"
         m%comment = "dispersive-term multiplier: 1 = fully dispersive, 0 = shallow-water; SWE, wet/dry and slope tapers"
      end select
   end function field_meta

end module model_field_metadata_mod
