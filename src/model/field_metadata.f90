! allow(E001)
! =================================================================
!  GENERATED FILE — DO NOT EDIT.
!  Source:    src/model/registry.yaml
!  Generator: scripts/gen_registry.py   (rerun after registry edits)
!  Sync test: scripts/gen_registry.py --check
! =================================================================
!> @file field_metadata.f90
!> @brief Generated CF attribute catalog for output field variables.
module model_field_metadata_mod
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
         m%long_name = "free surface elevation"
         m%standard_name = "sea_surface_height_above_mean_sea_level"
      case ("u")
         m%units = "m s-1"
         m%long_name = "depth-averaged x-velocity"
         m%standard_name = "eastward_sea_water_velocity"
      case ("v")
         m%units = "m s-1"
         m%long_name = "depth-averaged y-velocity"
         m%standard_name = "northward_sea_water_velocity"
      case ("p_flux")
         m%units = "m2 s-1"
         m%long_name = "x-component of depth-integrated volume flux"
      case ("q_flux")
         m%units = "m2 s-1"
         m%long_name = "y-component of depth-integrated volume flux"
      case ("depth")
         m%units = "m"
         m%long_name = "still-water depth"
         m%standard_name = "sea_floor_depth_below_mean_sea_level"
      case ("h")
         m%units = "m"
         m%long_name = "total water depth"
         m%standard_name = "sea_floor_depth_below_sea_surface"
      case ("mask")
         m%units = "1"
         m%long_name = "wet/dry mask (1=wet, 0=dry)"
      case ("mask9")
         m%units = "1"
         m%long_name = "wet/dry mask on 3x3 stencil (1=wet, 0=dry)"
      case ("h_max")
         m%units = "m"
         m%long_name = "maximum sea surface elevation above mean sea level"
      case ("h_min")
         m%units = "m"
         m%long_name = "minimum sea surface elevation above mean sea level"
      case ("u_max")
         m%units = "m s-1"
         m%long_name = "maximum depth-averaged sea water speed"
      case ("arr_time")
         m%units = "s"
         m%long_name = "wave front arrival time"
      end select
   end function field_meta

end module model_field_metadata_mod
