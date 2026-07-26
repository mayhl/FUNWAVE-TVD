module model_fields_2d_mod
   use core_constants_mod, only: SP, N_GHOST
   use core_grid_mod, only: type_grid_2d
   use core_field_registry_mod, only: type_field_registry
   implicit none

   !> Primary field arrays for the 2D Boussinesq model.
   !>
   !> All 2D arrays are ghost-inclusive:
   !>   dimension (local_nx + 2*N_GHOST, local_ny + 2*N_GHOST).
   !> Interior indices run (N_GHOST+1 : N_GHOST+local_nx, N_GHOST+1 : N_GHOST+local_ny).
   !> Call alloc(grid) after grid%setup() and grid%init_spacing().
   !>
   !> Naming follows registry.yaml (snake_case, matches output variable names).
   !> Legacy mod_global.F equivalents noted inline.
   type, public :: type_fields_2d

      ! ── Prognostic state ─────────────────────────────────────────────────
      ! Primary variables advanced by the time-stepping scheme.
      real(SP), allocatable :: eta(:, :)   !< free surface elevation           [m]    (ETA)
      real(SP), allocatable :: p(:, :)     !< x depth-integrated flux H*u      [m2/s] (Ubar)
      real(SP), allocatable :: q(:, :)     !< y depth-integrated flux H*v      [m2/s] (Vbar)

      ! ── Derived per-step ─────────────────────────────────────────────────
      ! Updated each step from prognostic state; needed by physics and output.
      real(SP), allocatable :: u(:, :)     !< depth-averaged x-velocity p/H    [m/s]  (U)
      real(SP), allocatable :: v(:, :)     !< depth-averaged y-velocity q/H    [m/s]  (V)
      real(SP), allocatable :: h(:, :)     !< total water depth eta+depth       [m]    (H)
      real(SP), allocatable :: hu(:, :)    !< cell-centred x volume flux H*u   [m2/s] (HU)
      real(SP), allocatable :: hv(:, :)    !< cell-centred y volume flux H*v   [m2/s] (HV)

      ! ── Bathymetry ───────────────────────────────────────────────────────
      ! Set at initialisation; static unless bed-deformation is enabled.
      real(SP), allocatable :: depth(:, :)      !< still-water depth            [m]  (Depth)
      real(SP), allocatable :: depth_node(:, :) !< depth at cell nodes          [m]  (DepthNode)
      real(SP), allocatable :: depth_x(:, :)    !< x-gradient of depth          [1]  (Depthx)
      real(SP), allocatable :: depth_y(:, :)    !< y-gradient of depth          [1]  (Depthy)

      ! ── Masks ────────────────────────────────────────────────────────────
      ! 1 = wet cell, 0 = dry cell.
      integer, allocatable :: mask(:, :)        !< wet/dry mask                 (MASK)
      integer, allocatable :: mask_struc(:, :)  !< permanent structure mask     (MASK_STRUC)
      integer, allocatable :: mask9(:, :)       !< 3x3 stencil wet/dry mask     (MASK9)
      ! real SWE dispersion weight: mask9 x smoothstep taper (swe_eta_ramp);
      ! equals real(mask9) when the taper is off — the kernels' multiplier
      real(SP), allocatable :: swe_w(:, :)      !< dispersion gate weight

      ! ── Runge-Kutta history ───────────────────────────────────────────────
      ! State saved at the start of each timestep for multi-stage RK.
      real(SP), allocatable :: eta0(:, :)  !< eta at previous time level       (Eta0)
      real(SP), allocatable :: p0(:, :)    !< p   at previous time level       (Ubar0)
      real(SP), allocatable :: q0(:, :)    !< q   at previous time level       (Vbar0)

      ! ── Diagnostic / envelope fields ─────────────────────────────────────
      ! Accumulated over the run; written at output time.
      real(SP), allocatable :: h_max(:, :)    !< maximum surface elevation     (HeightMax)
      real(SP), allocatable :: h_min(:, :)    !< minimum surface elevation     (HeightMin)
      real(SP), allocatable :: u_max(:, :)    !< maximum depth-averaged speed  (VelocityMax)
      real(SP), allocatable :: mf_max(:, :)   !< maximum momentum flux mag.    (MomentumFluxMax)
      real(SP), allocatable :: vort_max(:, :) !< maximum vorticity magnitude   (VorticityMax)
      real(SP), allocatable :: arr_time(:, :) !< wave front arrival time       (ARRTIME)

      ! ── Breaking physics (optional) ───────────────────────────────────────
      ! Allocated by alloc_breaking(); unallocated = breaking disabled.
      ! Velocity gradients are recomputed each step (same stencil as dispersion.F).
      real(SP), allocatable :: nu_break(:, :) !< breaking eddy viscosity        [m2/s]  (nu_break)
      real(SP), allocatable :: age_break(:, :)!< breaking-event age             [s]     (AGE_BREAKING)
      real(SP), allocatable :: ux(:, :)       !< du/dx                          [1/s]   (Ux)
      real(SP), allocatable :: uy(:, :)       !< du/dy                          [1/s]   (Uy)
      real(SP), allocatable :: vx(:, :)       !< dv/dx                          [1/s]   (Vx)
      real(SP), allocatable :: vy(:, :)       !< dv/dy                          [1/s]   (Vy)
      real(SP), allocatable :: d_break(:, :)  !< wave-breaking dissipation rate [W/m2]

   contains
      procedure :: alloc => fields_alloc
      procedure :: alloc_breaking => fields_alloc_breaking
      procedure :: register => fields_register
      procedure :: free => fields_free
   end type type_fields_2d

contains

   !> Allocate all field arrays based on local grid dimensions.
   subroutine fields_alloc(this, grid)
      class(type_fields_2d), intent(inout) :: this
      type(type_grid_2d), intent(in)    :: grid

      integer :: ng, nx, ny, mloc, nloc

      ng = N_GHOST
      nx = grid%local_nx
      ny = grid%local_ny
      mloc = nx + 2*ng
      nloc = ny + 2*ng

      call this%free()

      allocate (this%eta(mloc, nloc), source=0.0_SP)
      allocate (this%p(mloc, nloc), source=0.0_SP)
      allocate (this%q(mloc, nloc), source=0.0_SP)
      allocate (this%u(mloc, nloc), source=0.0_SP)
      allocate (this%v(mloc, nloc), source=0.0_SP)
      allocate (this%h(mloc, nloc), source=0.0_SP)
      allocate (this%hu(mloc, nloc), source=0.0_SP)
      allocate (this%hv(mloc, nloc), source=0.0_SP)

      allocate (this%depth(mloc, nloc), source=0.0_SP)
      allocate (this%depth_node(mloc, nloc), source=0.0_SP)
      allocate (this%depth_x(mloc, nloc), source=0.0_SP)
      allocate (this%depth_y(mloc, nloc), source=0.0_SP)

      allocate (this%mask(mloc, nloc), source=0)
      allocate (this%mask_struc(mloc, nloc), source=0)
      allocate (this%mask9(mloc, nloc), source=0)
      allocate (this%swe_w(mloc, nloc), source=0.0_SP)

      allocate (this%eta0(mloc, nloc), source=0.0_SP)
      allocate (this%p0(mloc, nloc), source=0.0_SP)
      allocate (this%q0(mloc, nloc), source=0.0_SP)

      allocate (this%h_max(mloc, nloc), source=0.0_SP)
      allocate (this%h_min(mloc, nloc), source=0.0_SP)
      allocate (this%u_max(mloc, nloc), source=0.0_SP)
      allocate (this%mf_max(mloc, nloc), source=0.0_SP)
      allocate (this%vort_max(mloc, nloc), source=0.0_SP)
      allocate (this%arr_time(mloc, nloc), source=0.0_SP)
   end subroutine fields_alloc

   !> Allocate breaking-physics arrays.  Call after alloc() when any breaker
   !> mode is active (viscosity, WAVEMAKER_VIS, or the show-only display mode).
   subroutine fields_alloc_breaking(this, grid)
      class(type_fields_2d), intent(inout) :: this
      type(type_grid_2d), intent(in)    :: grid

      integer :: ng, mloc, nloc

      ng = N_GHOST
      mloc = grid%local_nx + 2*ng
      nloc = grid%local_ny + 2*ng

      allocate (this%nu_break(mloc, nloc), source=0.0_SP)
      allocate (this%age_break(mloc, nloc), source=0.0_SP)
      allocate (this%ux(mloc, nloc), source=0.0_SP)
      allocate (this%uy(mloc, nloc), source=0.0_SP)
      allocate (this%vx(mloc, nloc), source=0.0_SP)
      allocate (this%vy(mloc, nloc), source=0.0_SP)
      allocate (this%d_break(mloc, nloc), source=0.0_SP)
   end subroutine fields_alloc_breaking

   !> Register all output-relevant fields with the field registry by their
   !> registry.yaml names.  Call once after alloc() (and alloc_breaking(),
   !> when active) in the init path; output channels then look fields up via
   !> registry%get(name).
   !>
   !> `this` must be a target: the registry stores pointers into these arrays.
   !> Integer masks (mask/mask_struc/mask9) are NOT registered — the registry
   !> is real(SP)-only; an integer channel or real view is deferred to Step 6.
   subroutine fields_register(this, registry)
      class(type_fields_2d), target, intent(in)    :: this
      type(type_field_registry), intent(inout) :: registry

      call registry%register("eta", this%eta)
      call registry%register("p", this%p)
      call registry%register("q", this%q)
      call registry%register("u", this%u)
      call registry%register("v", this%v)
      call registry%register("h", this%h)

      call registry%register("depth", this%depth)
      call registry%register("depth_node", this%depth_node)
      call registry%register("depth_x", this%depth_x)
      call registry%register("depth_y", this%depth_y)

      call registry%register("h_max", this%h_max)
      call registry%register("h_min", this%h_min)
      call registry%register("u_max", this%u_max)
      call registry%register("mf_max", this%mf_max)
      call registry%register("vort_max", this%vort_max)
      call registry%register("arr_time", this%arr_time)

      ! Optional breaking-physics fields: present only after alloc_breaking().
      if (allocated(this%nu_break)) call registry%register("nu_break", this%nu_break)
      if (allocated(this%age_break)) call registry%register("age_break", this%age_break)
      if (allocated(this%d_break)) call registry%register("d_break", this%d_break)
   end subroutine fields_register

   !> Deallocate all field arrays.
   subroutine fields_free(this)
      class(type_fields_2d), intent(inout) :: this

      if (allocated(this%eta)) deallocate (this%eta)
      if (allocated(this%p)) deallocate (this%p)
      if (allocated(this%q)) deallocate (this%q)
      if (allocated(this%u)) deallocate (this%u)
      if (allocated(this%v)) deallocate (this%v)
      if (allocated(this%h)) deallocate (this%h)
      if (allocated(this%hu)) deallocate (this%hu)
      if (allocated(this%hv)) deallocate (this%hv)

      if (allocated(this%depth)) deallocate (this%depth)
      if (allocated(this%depth_node)) deallocate (this%depth_node)
      if (allocated(this%depth_x)) deallocate (this%depth_x)
      if (allocated(this%depth_y)) deallocate (this%depth_y)

      if (allocated(this%mask)) deallocate (this%mask)
      if (allocated(this%mask_struc)) deallocate (this%mask_struc)
      if (allocated(this%mask9)) deallocate (this%mask9)
      if (allocated(this%swe_w)) deallocate (this%swe_w)

      if (allocated(this%eta0)) deallocate (this%eta0)
      if (allocated(this%p0)) deallocate (this%p0)
      if (allocated(this%q0)) deallocate (this%q0)

      if (allocated(this%h_max)) deallocate (this%h_max)
      if (allocated(this%h_min)) deallocate (this%h_min)
      if (allocated(this%u_max)) deallocate (this%u_max)
      if (allocated(this%mf_max)) deallocate (this%mf_max)
      if (allocated(this%vort_max)) deallocate (this%vort_max)
      if (allocated(this%arr_time)) deallocate (this%arr_time)

      if (allocated(this%nu_break)) deallocate (this%nu_break)
      if (allocated(this%age_break)) deallocate (this%age_break)
      if (allocated(this%ux)) deallocate (this%ux)
      if (allocated(this%uy)) deallocate (this%uy)
      if (allocated(this%vx)) deallocate (this%vx)
      if (allocated(this%vy)) deallocate (this%vy)
      if (allocated(this%d_break)) deallocate (this%d_break)
   end subroutine fields_free

end module model_fields_2d_mod
